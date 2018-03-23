000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400121109      * I barcodes sono orizzontalizzati a partire dalla posizione 257 in segmenti lunghi 14;
000500121109      * occorre gestirne 30 (ovvero dalla posizione 257 per 14 bytes x 30 barcodes).
000600121109      * Dei segmenti lunghi 14 occorre:
000700121109      * -       considerare solamente i primi 12 caratteri
000800121109      * -       tramite il *pgm di utilità TRUL28R4 (vedi suo utilizzo TRUL28R) occorre calcolare il
000900121109      *         check-digits secondo il modulo 'E13'
001000121109      * VABTIC: riportare TM qualora sia riportato il contrassegno.
001100121001      *
001200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001300990908
001400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001500021113     FFIVABwwr  O    E             DISK    usropn
001600021113     FFIVATwwr  O    E             DISK    usropn
001700990908
001800000801     D*----------------------------------------------------
001900000801     D* DICHIARAZIOINE VARIABILI DI WRK
002000000801     D*----------------------------------------------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500070730     D tisi95ds      e ds
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100121109     D depspe          s             10    INZ(*blanks)
003200121109     D curspe          s             10    INZ(*blanks)
003300010202     D parccm          s              8    INZ(*blanks)
003400010202     D parmbr          s             10    INZ(*blanks)
003500010202     D paropz          s              1    INZ(*blanks)
003600010202     D chkcall         s              1    INZ(*blanks)
003700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003800070530     D Num5_0          s              5  0
003900121011     D w70             s             70
004000000830
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600101007     D  VINDTA                     2048
004700101007     D  VINFLG                        1
004800121109     D  VINSPE                       10
004900101007     D  VINRRN                        8  0
005000020725     D*
005100080923     D*------------------
005200080923     D* DS REPERIMENTO NUMERATORE
005300080923     D*------------------
005400080923     D trul33ds      e ds                  inz
005500080923     D*------------------
005600080923     D* DS ARCHITETTURA
005700080923     D*------------------
005800080923     D kpjba         e ds                  inz
005900080923     D*------------------
006000121126     D BarcodeDS       DS           770    INZ
006100121126     D  wBarcode                     14    dim(55)
006200121109     D*------------------
006300121109     D TRUL28DSX     e ds                  inz
006400121109     D*------------------
006500121109     D wX              s              3s 0
006600081113
006700081113     D*------------------
006800081113     D* LINKING A DEFINIZIONI ESTERNE
006900081113     D*------------------
007000100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200081113
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008800000613     C*
008900010202     C* Effettuo la chiamata al CLLE preposto
009000050128     C                   call(e)   'TITVVTC'
009100010202     C                   parm                    parccm
009200010202     C                   parm                    parmbr
009300010202     C                   parm      '2'           paropz
009400070730     C*
009500070730     C* Effettuo lancio TISI95 solo x chiusura
009600070730     C                   CLEAR                   TISI95DS
009700070730     C                   EVAL      I95TLA = 'C'
009800070730     C                   CALL      'TISI95R'
009900070730     C                   PARM                    TISI95DS
010000000616     C*
010100000801     C
010200010201     C                   seton                                        LR
010300000801
010400910830     C*--------------------------------------------------------
010500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010600910830     C*--------------------------------------------------------
010700070530     C     RWFILE        BEGSR
010800990910     C*
010900990914     C                   if        not %open(tivin00r)
011000990908     C                   open      tivin00r
011100990914     C                   endif
011200021113     C                   if        not %open(fivabwwr)
011300021113     C                   open      fivabwwr
011400990914     C                   endif
011500070530     C*
011600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011700020305     C                   exsr      prevat
011800010201     C*
011900010202     C                   if        chkcall = '0'
012000010202     C*
012100021113     C                   if        not %open(fivatwwr)
012200021113     C                   open      fivatwwr
012300010201     C                   endif
012400080117     C*
012500080117     C                   EXSR      INZVAR
012600080117     C                   EXSR      DEFCAM
012700990910     C*
012800010201     C                   clear                   §CTROKVB          5 0
012900020305     C                   clear                   §CTROKVT          5 0
013000000801     C                   clear                   §CTRMO            5 0
013100000801     C                   clear                   §CTRNO            5 0
013200990910     C*
013300020725     C/EXEC SQL
013400020725     C+ declare C1 cursor for select
013500121109     C+ vindta, vinflg, substr(vindta, 108, 10) as sped, rrn(tivin00r)
013600060223     C+ from tivin00r
013700060223     C+ order by sped
013800020725     C+ for read only
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200020725     C+ open C1
014300020725     C/END-EXEC
014400020725     C
014500020725     C/EXEC SQL
014600070530     C+ Fetch C1 into :INPUT_DS
014700020725     C/END-EXEC
014800020725     C*
014900020725     C                   dow       sqlcod = *zeros
015000990913     C*
015100020725     C                   if        vindta > *blanks
015200000613     C                   add       1             rrnum
015300000801     C*
015400020725     C                   if        vinflg = *blanks
015500020725     C                             or vinflg = '0'
015600020725     C                             or vinflg = '2'
015700000801     C*
015800020725     C                   clear                   x_vinmsg
015900020725     C                   eval      x_vinflg = '1'
016000010305     C*
016100121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016200121109     C* (uso l'RMN)
016300121109     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
016400020305     C                   MOVEL(p)  PiStr         curspe
016500010305     C*
016600071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800071003     C                   exsr      impvab
016900071003     C                   exsr      wrivab
017000121001     C***                exsr      wrivat_b                                     => carico VAT
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400071009     C                   exsr      impvab
017500071009     C                   exsr      wrivab
017600121001     C***                exsr      wrivat_b                                     => carico VAT
017700121109     C                   exsr      wrivat_e                                     => carico VAT
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300020305     C                   exsr      impvab
018400121001     C***                exsr      wrivat_b                                     => carico VAT
018500121109     C   50              exsr      wrivat_e                                     => carico VAT
018600010305     C                   else
018700020725     C                   if        depspe <> curspe                             => rottura di spediz
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900070928     C                   exsr      wrivab
019000080117     C                   clear                   tivinds
019100020305     C                   exsr      impvab
019200121001     C***                exsr      wrivat_b                                     => carico VAT
019300121109     C   50              exsr      wrivat_e                                     => carico VAT
019400020305     C                   else                                                   => x stessa spediz
019500090204     C***                exsr      impvab
019600121001     C***                exsr      wrivat_b                                     => carico VAT
019700121109     C   50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100071003     C                   endif
020200071009     C                   endif
020300000905     C*
020400000905     C                   else
020500020725     C                   eval      x_vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800020725     C     VINRRN        chain     tivin000
020900020725     C                   if        %found(tivin00r)
021000020725     C                   eval      y_vinflg = x_vinflg
021100020725     C                   eval      y_vinmsg = x_vinmsg
021200020725     C                   update    tivin000
021300020725     C                   endif
021400020725     C*
021500020725     C/EXEC SQL
021600070530     C+ Fetch C1 into :INPUT_DS
021700020725     C/END-EXEC
021800020725     C*
021900020725     C                   enddo
022000020725     C*
022100020725     C/EXEC SQL
022200020725     C+ close C1
022300020725     C/END-EXEC
022400000905     C*
022500020305     C* Scarico i VAB rimasti "in sospeso"
022600071009     C                   if        wDISK = 'C'
022700071009     C                   exsr      wrivab
022800071009     C                   endif
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700010201     C                   if        §ctrokvb > 0
023800990921     C                   eval      wrkesito = '1'
023900000710     C                   else
024000000710     C                   eval      wrkesito = '2'
024100990910     C                   endif
024200000710     C                   endif
024300990910     C*
024400990914     C                   if        %open(tivin00r)
024500990908     C                   close     tivin00r
024600990914     C                   endif
024700021113     C                   if        %open(fivabwwr)
024800021113     C                   close     fivabwwr
024900990914     C                   endif
025000021113     C                   if        %open(fivatwwr)
025100021113     C                   close     fivatwwr
025200010201     C                   endif
025300990910     C*
025400010201     C                   if        §ctrokvb > 0
025500000724     C                             and vlrpoi <> *zeros
025600010202     C                   exsr      invio
025700990920     C                   endif
025800990920     C*
025900910830     C                   ENDSR
026000000613     C***
026100010305
026200010305     C*----------------------------------------------------*
026300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026400010305     C*----------------------------------------------------*
026500020305     C     WRIVAB        BEGSR
026600070730     C*
026700071003     C* Considerazioni finali
026800071003     C                   if        VABRMA = *blanks
026900071003     C                   movel(P)  VABRMN        VABRMA
027000071003     C                   endif
027100071003     C*
027200021113     C                   write     fivab000                                     => scarico il VAB
027300010305     C*
027400010305     C                   ENDSR
027500990920
027600000801     C*----------------------------------------------------*
027700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027800000801     C*----------------------------------------------------*
027900010201     C     INZVAR        BEGSR
028000000801     C*
028100010201     C                   Z-ADD     *zeros        Num5_0
028200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028300020725     C                   MOVEL     '0'           FlgCAS            1
028400000801     C*
028500000801     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028800000801     C*----------------------------------------------------*
028900020904     C     DEFCAM        BEGSR
029000080923     C*
029100080923     C                   SETOFF                                       5051
029200000801     C*
029300021113     C                   CLEAR                   FIVAB000
029400021113     C                   CLEAR                   FIVAT000
029500070730     C* Imposto i valori di default...
029600121109     C                   EVAL      VABCCM = 0521735
029700121109     C                   EVAL      VATCCM = 0521735
029800121109     C                   EVAL      VABLNP = 066
029900121109     C                   EVAL      VATLNP = 066
030000121109     C                   EVAL      VABCTR = 100
030100121109     C                   EVAL      VABTSP = 'D'
030200121011     C                   EVAL      VABCBO = '1'
030300070222     C* ... e poi verifico se sono stati passati come parametri
030400070222     C                   IF        vlrppt > *blanks
030500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030700070222     C                   EXSR      CHKNUM
030800070222     C                   IF        PiInt=*on
030900070222     C                   Z-ADD     PiVal         VABCCM
031000070222     C                   Z-ADD     PiVal         VATCCM
031100070222     C                   ENDIF
031200070222     C                   ENDIF
031300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABLNP
031800070222     C                   Z-ADD     PiVal         VATLNP
031900070222     C                   ENDIF
032000070222     C                   ENDIF
032100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032300070222     C                   EXSR      CHKNUM
032400070222     C                   IF        PiInt=*on
032500070222     C                   Z-ADD     PiVal         VABCTR
032600070222     C                   ENDIF
032700070928     C                   ENDIF
032800071009     C                   MOVEL     *blanks       wDISK             1
032900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033000071009     C                   IF        wDISK <> *blanks
033100070928     C                   SETON                                        50
033200070222     C                   ENDIF
033300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033400080923     C                   SETON                                        51
033500080923     C                   ENDIF
033600070222     C                   ENDIF
033700071009     C*
033800071009     C   50              EVAL      VABCTM = '7Q'
033900000801     C*
034000000801     C                   ENDSR
034100000801     C*----------------------------------------------------*
034200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034300000801     C*----------------------------------------------------*
034400010201     C     IMPVAB        BEGSR
034500070530     C*
034600070530     C                   SETOFF                                       3132
034700070928     C*
034800070928     C                   EXSR      INZVAR
034900070928     C                   EXSR      DEFCAM
035000070928     C*
035100070928     C                   MOVE(P)   vlrpoi        VABFGS
035200070928     C                   MOVE(P)   vlrpoi        VATFGS
035300070928     C*
035400070928     C                   MOVEL     datcor        VABAAS
035500070928     C                   MOVEL     datcor        VATAAS
035600070928     C                   MOVE      datcor        VABMGS
035700121109     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
035800070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035900070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036000070928     C* ==
036100121109     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
036200121109     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
036300121109     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
036400121109     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
036500121109     C                   EVAL      VABCBO='1'
036600121109     C                   IF        %trim(%subst(vindta:207:1)) = 'A'
036700121109     C                   EVAL      VABCBO='2'
036800121109     C                   ENDIF
036900120717     C*
037000071003     C* CAD
037100121109     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
037200121001     C                   EXSR      CHKNUM
037300121001     C                   IF        PiInt=*on
037400121001     C                   Z-ADD     PiVal         Num5_0
037500121001     C                   MOVEL     Num5_0        VABCAD
037600121001     C                   ELSE
037700121001     C                   SETON                                        32
037800121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037900121001     C                             + ' ' + 'VABCAD'
038000121001     C                   ENDIF
038100080923     C* RMN
038200121109     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
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
042400121109     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
042500070928     C                   EXSR      CHKNUM
042600070928     C                   IF        PiInt=*on
042700070928     C                   Z-ADD     PiVal         VABNCL
042800070928     C                   ELSE
042900070928     C                   SETON                                        32
043000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100070928     C                             + ' ' + 'VABNCL'
043200070928     C                   ENDIF
043300071003     C* PKB
043400121109     C                   EVAL      PiStr=%trim(%subst(vindta:129:8))
043500070928     C                   EXSR      CHKNUM
043600070928     C                   IF        PiNum=*on
043700121109     C* il dato non ha decimali
043800121109     C                   EVAL(H)   VABPKB = PiVal
043900070928     C                   ELSE
044000070928     C                   SETON                                        32
044100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200070928     C                             + ' ' + 'VABPKB'
044300070928     C                   ENDIF
044400080923     C* CAS
044500121109     C* l'importo contrassegno ha 2 decimali e viene scritto con separatore decimale
044600121109     C                   IF        %trim(%subst(vindta:137:10))<>*blanks
044700080923     C                             AND
044800121109     C                             %trim(%subst(vindta:137:10))<>*zeros
044900121011     C                             AND
045000121109     C                             %trim(%subst(vindta:137:10))<>
045100121109     C                              '0000000,00'
045200121109     C                             AND
045300121109     C                             %trim(%subst(vindta:137:10))<>
045400121109     C                              '      0,00'
045500121011     C                   EVAL      VABVCA = 'EUR'
045600070928     C                   MOVEL     '1'           FlgCAS
045700121109     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
045800070928     C                   EXSR      CHKNUM
045900070928     C                   IF        PiNum=*on
046000121109     C                   EVAL(H)   VABCAS = PiVal
046100070928     C                   ELSE
046200070928     C                   SETON                                        32
046300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046400070928     C                             + ' ' + 'VABCAS'
046500070928     C                   ENDIF
046600070928     C                   ENDIF
046700070730     C*
046800121109     C* Considerazioni finali su CBO/CAS
046900121106     C*
047000121106      * se c/assegno impostato
047100121106     C                   IF        VABCAS > 0
047200121109      * metto TM come tipo incasso
047300121109     C                   EVAL      VABTIC = 'TM'
047400121109     C                   IF        VABCBO = '1'
047500121109     C                   EVAL      VABCBO = '4'
047600121109     C                   ENDIF
047700121109     C                   IF        VABCBO = '2'
047800121109     C                   EVAL      VABCBO = '6'
047900121109     C                   ENDIF
048000121106     C                   ENDIF
048100020305     C*
048200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048300011113     C                   EXSR      CHKIMPDIV
048400010202     C*
048500000801     C* Ebbene...
048600000801     C                   ADD       1             §CTRMO
048700070530     C                   IF        *in31 <> *zeros OR
048800070530     C                             *in32 <> *zeros
048900000801     C                   ADD       1             §CTRNO
049000020725     C                   EVAL      x_vinflg = '2'
049100000801     C                   ELSE
049200010201     C                   ADD       1             §CTROKVB
049300000801     C                   ENDIF
049400000801     C*
049500000801     C                   ENDSR
049600010201     C*----------------------------------------------------*
049700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
049800010201     C*----------------------------------------------------*
049900071003     C     WRIVAT_B      BEGSR
050000090204     C*
050100090204     C                   eval      vatAAS = vabAAS
050200090204     C                   eval      vatLNP = vabLNP
050300090204     C                   eval      vatNRS = vabNRS
050400090204     C                   eval      vatNSP = vabNSP
050500010201     C*
050600101007     C* Valorizzo il buffer di scrittura del FIVAT
050700070928     C* - TIPO RECORD "B"
050800070928     C                   if        %subst(vindta:92:15) <> *blanks
050900070928     C                   eval      VATTRC = 'B'
051000121001     C                   eval      VATNOT = %trim(%subst(vindta:01:01))
051100021113     C                   write     FIVAT000
051200020725     C                   endif
051300010201     C*
051400010201     C                   ENDSR
051500071003     C*----------------------------------------------------*
051600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
051700121109      * I barcodes sono orizzontalizzati a partire dalla posizione 257 in segmenti lunghi 14;
051800121109      * occorre gestirne 30 (ovvero dalla posizione 257 per 14 bytes x 30 barcodes).
051900121109      * Dei segmenti lunghi 14 occorre:
052000121109      * -       considerare solamente i primi 12 caratteri
052100121109      * -       tramite il *pgm di utilità TRUL28R4 (vedi suo utilizzo TRUL28R) occorre calcolare il
052200121109      *         check-digits secondo il modulo 'E13'
052300071003     C*----------------------------------------------------*
052400071003     C     WRIVAT_E      BEGSR
052500090204     C*
052600090204     C                   eval      vatAAS = vabAAS
052700090204     C                   eval      vatLNP = vabLNP
052800090204     C                   eval      vatNRS = vabNRS
052900090204     C                   eval      vatNSP = vabNSP
053000121109     C                   eval      VATTRC = 'E'
053100071003     C*
053200101007     C* Valorizzo il buffer di scrittura del FIVAT
053300071003     C* - TIPO RECORD "E"
053400121126     C                   if        %subst(vindta:257:770) <> *blanks
053500121109     C*
053600121109     C                   RESET                   BarcodeDS
053700121126     C                   eval      BarcodeDS = %trim(%subst(vindta:257:770))
053800121109     C*
053900121126     C     1             DO        55            wX
054000121109     C                   IF        wBarcode(wX) <> *blank
054100121109     C                   eval      VATNOT = %trim(%subst(wBarcode(wX) : 1 : 12))
054200121109     C* recupero il check digit
054300121109     C                   clear                   TRUL28DSX
054400121109     C                   eval      I284STD = 'E13'
054500121109     C                   eval      I284COD = VATNOT
054600121109     C                   call      'TRUL28R4'
054700121109     C                   parm                    TRUL28DSX
054800121109     C* Se tutto ok scrivo il "chi sono" corrente
054900121109     C                   IF        O284err = *blanks
055000121109     C                   EVAL      VATNOT = %trim(VATNOT) + %trim(O284CkD)
055100071003     C                   write     FIVAT000
055200121109     C                   endif
055300121109     C                   endif
055400121109     C                   enddo
055500121109     C*
055600071003     C                   endif
055700071003     C*
055800071003     C                   ENDSR
055900020904
056000020904
056100010202     C*----------------------------------------------------*
056200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056300010202     C*----------------------------------------------------*
056400020305     C     PREVAT        BEGSR
056500010202     C*
056600021113     C* Compongo il nome del membro da dare al FIVATWWR
056700010202     C                   eval      parmbr = vlrhdl
056800010202     C                   movel     'M'           parmbr
056900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057000010202     C                   eval      paropz = '1'
057100010202     C* Effettuo la chiamata al CLLE preposto
057200050128     C                   call(e)   'TITVVTC'
057300010202     C                   parm                    parccm
057400010202     C                   parm                    parmbr
057500010202     C                   parm                    paropz
057600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057700010202     C                   if        %error
057800010202     C                   movel     '1'           chkcall
057900010202     C                   else
058000010202     C                   movel     '0'           chkcall
058100010202     C                   endif
058200010202     C*
058300010202     C                   ENDSR
058400000801     C*----------------------------------------------------*
058500000801     C*  CONTROLLO NUMERICITA' CAMPI
058600000801     C*----------------------------------------------------*
058700000801     C     CHKNUM        BEGSR
058800081113     C*
058900081113     C                   IF        PiDecChr = *blanks
059000121109     C                   EVAL      PiDecChr = ','
059100081113     C                   ENDIF
059200081113     C*
059300081113     C                   callp(e)  UBISNUM_Check(PiStr
059400081113     C                                          :PiDecChr
059500081113     C                                          :PiVal
059600081113     C                                          :PiNum
059700081113     C                                          :PiInt)
059800081113     C*
059900000801     C                   IF        %error
060000000801     C                   EVAL      PiInt=*off
060100000801     C                   ENDIF
060200000801     C*
060300000801     C                   ENDSR
060400000801     C***
060500000801
060600011113
060700011113     C*----------------------------------------------------*
060800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060900011113     C*----------------------------------------------------*
061000011113     C     CHKIMPDIV     BEGSR
061100011113     C*
061200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061300011113     C                   Z-ADD     *zeros        wrkDec            9 9
061400011113     C*
061500011113     C* Come prima cosa effettuo considerazioni sulla divisa
061600011113     C                   IF        vabIAS > *zeros
061700011113     C                   IF        vabVAS <> 'EUR'
061800011113     C                   EVAL      vabVAS =  'ITL'
061900011113     C                   ENDIF
062000011113     C                   ENDIF
062100011113     C*
062200011113     C                   IF        vabCAS > *zeros
062300011113     C                   IF        vabVCA <> 'EUR'
062400011113     C                   EVAL      vabVCA =  'ITL'
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   IF        vabVMD > *zeros
062900020305     C                   IF        vabVAD <> 'EUR'
063000011113     C                   EVAL      vabVAD =  'ITL'
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063500011113     C                   Z-ADD     vabIAS        wrkDec
063600011113     C                   IF        wrkDec > *zeros
063700011113     C                   IF        vabVAS = 'ITL'
063800011113     C                   EVAL      vabIAS = *zeros
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064300011113     C                   Z-ADD     vabCAS        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVCA = 'ITL'
064600011113     C                   EVAL      vabCAS = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065100011113     C                   Z-ADD     vabVMD        wrkDec
065200011113     C                   IF        wrkDec > *zeros
065300011113     C                   IF        vabVAD = 'ITL'
065400011113     C                   EVAL      vabVMD = *zeros
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C                   ENDSR
065900011113     C***
066000011113
066100011113
066200000801
066300000801
066400990920      /TITLE Invio dei dati al punto operativo.
066500010202     C     invio         BEGSR
066600990920     C*
066700021113     C* 1° invio FIVAT
066800010201     C                   reset                   dscmz
066900010201     C                   move      vlrpoi        cmzdst
067000021113     C                   eval      cmzfld = 'FIVATWWR'
067100010201     C                   eval      cmzmbd = vlrhdl
067200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067300021009     C***                if        prmfir = *blanks
067400021113     C                   eval      cmzfla = 'FIVAT00F'
067500021113     C                   eval      cmzmba = 'FIVAT00F'
067600021009     C***                else
067700021009     C***                eval      cmzfla = prmfir
067800021009     C***                eval      cmzmba = prmfir
067900021009     C***                endif
068000010201     C                   eval      cmznrr = *zeros
068100020305     C                   move      §ctrokvt      cmznrr
068200021018     C                   eval      cmzlba = vlrfl1
068300010201     C                   call(e)   'TIS711C'
068400010201     C                   parm                    dscmz
068500010201     C                   parm      *blanks       esito
068600010205     C                   if        %error
068700010205     C                             or cmzerr = '1'
068800010205     C                             or esito  = '1'
068900010205     C                   eval      wrkesito = '3'
069000010205     C                   else
069100010201     C*
069200021113     C* 2° invio FIVAB
069300010201     C                   reset                   dscmz
069400010201     C                   move      vlrpoi        cmzdst
069500010201     C                   eval      cmzfld = vlrfou
069600010201     C                   eval      cmzmbd = vlrhdl
069700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069800021009     C***                if        prmfir = *blanks
069900021113     C                   eval      cmzfla = 'FIVAB00F'
070000021113     C                   eval      cmzmba = 'FIVAB00F'
070100021009     C***                else
070200021009     C***                eval      cmzfla = prmfir
070300021009     C***                eval      cmzmba = prmfir
070400021009     C***                endif
070500010201     C                   eval      cmznrr = *zeros
070600010201     C                   move      §ctrokvb      cmznrr
070700021018     C                   eval      cmzlba = vlrfl1
070800010201     C                   call(e)   'TIS711C'
070900010201     C                   parm                    dscmz
071000010201     C                   parm      *blanks       esito
071100010201     C                   if        %error
071200010201     C                             or cmzerr = '1'
071300010201     C                             or esito  = '1'
071400010201     C                   eval      wrkesito = '3'
071500010201     C                   endif
071600010205     C                   endif
071700990920     C*
071800000613     C                   ENDSR
071900000613     C***
072000070411
072100070411     C     *pssr         BEGSR
072200070411     C*
072300070411     C                   if        %open(tivin00r)
072400070411     C                   close     tivin00r
072500070411     C                   endif
072600070411     C                   if        %open(fivabwwr)
072700070411     C                   close     fivabwwr
072800070411     C                   endif
072900070411     C                   if        %open(fivatwwr)
073000070411     C                   close     fivatwwr
073100070411     C                   endif
073200070411     C*
073300070411     C* Effettuo la chiamata al CLLE preposto
073400070411     C                   call(e)   'TITVVTC'
073500070411     C                   parm                    parccm
073600070411     C                   parm                    parmbr
073700070411     C                   parm      '2'           paropz
073800070411     C*
073900070411     C                   eval      wrkesito = '2'
074000070411     C*
074100070411     C                   seton                                        LR
074200070411     C*
074300070411     C                   ENDSR     '*CANCL'
074400070411     C***
074500070411
074600990910
074700000613     C     *inzsr        BEGSR
074800990910     C*
074900990910     C     *entry        plist
075000990920     C                   parm                    tivlrds
075100990921     C                   parm      wrkesito      esito
075200000724     C                   parm                    prmlit
075300000710     C                   parm                    prmfir
075400000613     C*
075500000830     C* CALCOLA LA DATA CORRENTE
075600100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
075700100324     C                   eval      datcor = %dec(%date() : *ISO)
075800000830     C*
075900000613     C                   ENDSR
076000000613     C***
