000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140206     H dftactgrp(*no) actgrp(*caller) bnddir('TRUL')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021113     FFIVABwwr  O    E             DISK    usropn
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
001600131204     D fivabds       e ds                  extname(fivab00f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200070928     D depspe          s             16    INZ(*blanks)
002300070928     D curspe          s             16    INZ(*blanks)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002900070530     D Num5_0          s              5  0
003000000830
003100000830     D*------------------
003200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003300000830     D*------------------
003400000830     D WLBDA8          DS                  INZ
003500000830     D  G08DAT                 1      8  0
003600000830     D  G08INV                 9     16  0
003700000830     D  G08ERR                17     17
003800000830     D  G08TGI                18     22  0
003900000830     D*
004000020725
004100020725     D*------------------
004200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004300020725     D*------------------
004400070530     D INPUT_DS        DS                  INZ
004500020725     D  VINDTA                 1   2048
004600020725     D  VINFLG              2049   2049
004700020725     D  VINSPE              2050   2059
004800020725     D  VINRRN              2060   2067  0
004900090112     D*------------------
005000090112     D* DS REPERIMENTO NUMERATORE
005100090112     D*------------------
005200090112     D trul33ds      e ds                  inz
005300090112     D*------------------
005400090112     D* DS ARCHITETTURA
005500090112     D*------------------
005600090112     D kpjba         e ds                  inz
005700990908
005800010201
005900010201
006000071120     C*
006100071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006200071120     C
006300071120     C/EXEC SQL
006400071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006500071120     C/END-EXEC
006600071120     C*
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000613     C*
007100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007200000613     C*
007300010202     C* Effettuo la chiamata al CLLE preposto
007400050128     C                   call(e)   'TITVVTC'
007500010202     C                   parm                    parccm
007600010202     C                   parm                    parmbr
007700010202     C                   parm      '2'           paropz
007800170801     C*
007900170801     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
008000170801     C                   if        *in53
008100170801     C                   call      'TIS7P2SR'
008200170801     C                   parm      'C'           pIn_Opz           1
008300170801     C                   parm                    tivlrds
008400170801     C                   parm                    FIVABDS
008500170801     C                   parm      *blanks       pIn_CdIdAz        2
008600170801     C                   parm      *blanks       pIn_MaskPDF      50
008700170801     C                   parm      *blanks       pOut_Esito        1
008800170801     C                   endif
008900070730     C*
009000070730     C* Effettuo lancio TISI95 solo x chiusura
009100070730     C                   CLEAR                   TISI95DS
009200070730     C                   EVAL      I95TLA = 'C'
009300070730     C                   CALL      'TISI95R'
009400070730     C                   PARM                    TISI95DS
009500000616     C*
009600000801     C
009700010201     C                   seton                                        LR
009800000801
009900910830     C*--------------------------------------------------------
010000070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010100910830     C*--------------------------------------------------------
010200070530     C     RWFILE        BEGSR
010300990910     C*
010400990914     C                   if        not %open(tivin00r)
010500990908     C                   open      tivin00r
010600990914     C                   endif
010700021113     C                   if        not %open(fivabwwr)
010800021113     C                   open      fivabwwr
010900990914     C                   endif
011000131204     C*
011100131204     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
011200131204     C                   if        *in53
011300131204     C                   call      'TIS7P2SR'
011400131204     C                   parm      'O'           pIn_Opz           1
011500131204     C                   parm                    tivlrds
011600131204     C                   parm                    FIVABDS
011700131204     C                   parm      *blanks       pIn_CdIdAz        2
011800131204     C                   parm      *blanks       pIn_MaskPDF      50
011900131204     C                   parm      *blanks       pOut_Esito        1
012000131204     C                   endif
012100070530     C*
012200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012300020305     C                   exsr      prevat
012400010201     C*
012500010202     C                   if        chkcall = '0'
012600010202     C*
012700021113     C                   if        not %open(fivatwwr)
012800021113     C                   open      fivatwwr
012900010201     C                   endif
013000990910     C*
013100010201     C                   clear                   §CTROKVB          5 0
013200020305     C                   clear                   §CTROKVT          5 0
013300000801     C                   clear                   §CTRMO            5 0
013400000801     C                   clear                   §CTRNO            5 0
013500990910     C*
013600020725     C/EXEC SQL
013700020725     C+ declare C1 cursor for select
013800070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
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
016400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016500070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
016600020305     C                   MOVEL(p)  PiStr         curspe
016700010305     C*
016800010305     C                   if        depspe = *blanks                             => 1° giro
016900010305     C                   eval      depspe = curspe                              => memorizz. spediz
017000020725     C                   clear                   tivinds
017100090112     C                   exsr      repNSP
017200020305     C                   exsr      impvab
017300070928     C   50              exsr      wrivat                                       => carico VAT
017400010305     C                   else
017500020725     C                   if        depspe <> curspe                             => rottura di spediz
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700070928     C                   exsr      wrivab
017800020725     C                   clear                   tivinds
017900090112     C                   exsr      repNSP
018000020305     C                   exsr      impvab
018100070928     C   50              exsr      wrivat                                       => carico VAT
018200020305     C                   else                                                   => x stessa spediz
018300020305     C                   exsr      impvab
018400070928     C   50              exsr      wrivat                                       => carico VAT
018500010305     C                   endif
018600010305     C                   endif
018700010305     C                   endif
018800000905     C*
018900000905     C                   else
019000020725     C                   eval      x_vinflg = '1'
019100000905     C                   endif
019200000905     C*
019300020725     C     VINRRN        chain     tivin000
019400020725     C                   if        %found(tivin00r)
019500020725     C                   eval      y_vinflg = x_vinflg
019600020725     C                   eval      y_vinmsg = x_vinmsg
019700020725     C                   update    tivin000
019800020725     C                   endif
019900020725     C*
020000020725     C/EXEC SQL
020100070530     C+ Fetch C1 into :INPUT_DS
020200020725     C/END-EXEC
020300020725     C*
020400020725     C                   enddo
020500020725     C*
020600020725     C/EXEC SQL
020700020725     C+ close C1
020800020725     C/END-EXEC
020900000905     C*
021000020305     C* Scarico i VAB rimasti "in sospeso"
021100020305     C                   exsr      wrivab
021200010202     C*
021300010202     C                   endif
021400990910
021500990910     C* Se non ci sono record con errori ...
021600000710     C                   if        §ctrno = 0
021700990910     C* ... restituisco esito OK.
021800990921     C                   eval      wrkesito = '0'
021900990910     C                   else
022000010201     C                   if        §ctrokvb > 0
022100990921     C                   eval      wrkesito = '1'
022200000710     C                   else
022300000710     C                   eval      wrkesito = '2'
022400990910     C                   endif
022500000710     C                   endif
022600990910     C*
022700990914     C                   if        %open(tivin00r)
022800990908     C                   close     tivin00r
022900990914     C                   endif
023000021113     C                   if        %open(fivabwwr)
023100021113     C                   close     fivabwwr
023200990914     C                   endif
023300021113     C                   if        %open(fivatwwr)
023400021113     C                   close     fivatwwr
023500010201     C                   endif
023600990910     C*
023700010201     C                   if        §ctrokvb > 0
023800000724     C                             and vlrpoi <> *zeros
023900010202     C                   exsr      invio
024000990920     C                   endif
024100990920     C*
024200910830     C                   ENDSR
024300000613     C***
024400010305
024500010305     C*----------------------------------------------------*
024600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024700010305     C*----------------------------------------------------*
024800020305     C     WRIVAB        BEGSR
024900131204     C*
025000131204     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
025100131204     C                   if        *in53
025200131204     C                   if        not *in31
025300131204     C*
025400140228     C                   eval      pIn_MaskPDF = '&RMA*.pdf'
025500131204     C                   call      'TIS7P2SR'
025600131204     C                   parm      'W'           pIn_Opz           1
025700131204     C                   parm                    tivlrds
025800131204     C                   parm                    FIVABDS
025900131204     C                   parm      '10'          pIn_CdIdAz        2
026000131204     C                   parm                    pIn_MaskPDF      50
026100131204     C                   parm      *blanks       pOut_Esito        1
026200131204     C*
026300131204     C* Forzature:
026400131204     C*
026500131204     C* CTM
026600131204     C                   if        wCTM <> *blanks
026700131204     C                   eval      VABCTM = wCTM
026800131204     C                   endif
026900131204     C*
027000131204     C                   endif
027100131204     C                   endif
027200131204     C*
027300131204     C                   write     fivab000                                     => scarico il VAB
027400010305     C*
027500010305     C                   ENDSR
027600990920
027700000801     C*----------------------------------------------------*
027800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900000801     C*----------------------------------------------------*
028000010201     C     INZVAR        BEGSR
028100000801     C*
028200010201     C                   Z-ADD     *zeros        Num5_0
028300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028400020725     C                   MOVEL     '0'           FlgCAS            1
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028900000801     C*----------------------------------------------------*
029000020904     C     DEFCAM        BEGSR
029100000801     C*
029200021113     C                   CLEAR                   FIVAB000
029300021113     C                   CLEAR                   FIVAT000
029400070730     C* Imposto i valori di default...
029500131204     C                   EVAL      VABCCM = 0016926
029600131204     C                   EVAL      VATCCM = 0016926
029700131204     C                   EVAL      VABLNP = 001
029800131204     C                   EVAL      VATLNP = 001
029900070928     C                   EVAL      VABCTR = 000
030000070730     C                   EVAL      VABCBO = '1'
030100070928     C                   EVAL      VABCTM = '7Q'
030200070222     C* ... e poi verifico se sono stati passati come parametri
030300070222     C                   IF        vlrppt > *blanks
030400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABCCM
030900070222     C                   Z-ADD     PiVal         VATCCM
031000070222     C                   ENDIF
031100070222     C                   ENDIF
031200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABLNP
031700070222     C                   Z-ADD     PiVal         VATLNP
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABCTR
032500070222     C                   ENDIF
032600070928     C                   ENDIF
032700070928     C                   IF        %subst(vlrppt:14:1) = 'C'
032800070928     C                   SETON                                        50
032900070928     C                   ELSE
033000070928     C                   SETOFF                                       50
033100070222     C                   ENDIF
033200131204     C*
033300131204     C* Verifico se sui parametri del traduttore è richiesto la scrittura forzata del
033400131204     C* tipo record 'P' del VAT (packing list PDF)
033500131204     C                   if        %subst(vlrppt:15:1) = 'S'
033600131204     C                   seton                                        53
033700131204     C                   else
033800131204     C                   setoff                                       53
033900131204     C                   endif
034000131204     C*
034100131204     C* Verifico se sui parametri del traduttore è richiesto un particolare codice trattamento merce
034200131204     C                   if        %subst(vlrppt:16:2) <> *blanks
034300131204     C                   movel     *blanks       wCTM              2
034400131204     C                   eval      wCTM = %subst(vlrppt:16:2)
034500131204     C                   endif
034600131204     C*
034700070222     C                   ENDIF
034800000801     C*
034900000801     C                   ENDSR
035000090112     C*----------------------------------------------------*
035100090112     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035200090112     C*----------------------------------------------------*
035300090112     C     REPNSP        BEGSR
035400090112     C*
035500090112     C                   EXSR      INZVAR
035600090112     C                   EXSR      DEFCAM
035700090112     C*
035800090112     C* NSP => Stacco un numeratore da AZNUM
035900090112     C                   clear                   TRUL33DS
036000090112     C                   eval      I33OPE = *zeros
036100090112     C                   eval      I33CNU = 302
036200090112     C                   eval      I33NUM = 1
036300090112     C                   movel     TRUL33DS      KPJBU
036400090112     C                   call      'TRUL33R'
036500090112     C                   parm                    KPJBA
036600090112     C                   movel     KPJBU         TRUL33DS
036700090112     C                   if        O33ERR = *zeros
036800090112     C                   z-add     O33NRF        VABNSP
036900090112     C                   z-add     O33NRF        VATNSP
037000090112     C                   else
037100090112     C                   SETON                                        31
037200090112     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037300090112     C                             + ' ' + 'VABNSP VATNSP'
037400090112     C                   endif
037500090112     C*
037600090112     C                   ENDSR
037700000801     C*----------------------------------------------------*
037800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037900000801     C*----------------------------------------------------*
038000010201     C     IMPVAB        BEGSR
038100070530     C*
038200070530     C                   SETOFF                                       3132
038300070928     C*
038400070928     C                   MOVE(P)   vlrpoi        VABFGS
038500070928     C                   MOVE(P)   vlrpoi        VATFGS
038600070928     C*
038700070928     C                   MOVEL     datcor        VABAAS
038800070928     C                   MOVEL     datcor        VATAAS
038900070928     C                   MOVE      datcor        VABMGS
039000070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
039100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
039200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
039300070928     C* ==
039400070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
039500070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
039600070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
039700141023     C***                EVAL      VABRMA=%trim(%subst(vindta:315:16))
039800141023     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
039900070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
040000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
040100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
040200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
040300070928     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
040400070928     C                   EVAL      VABTSP='E'
040500070928     C                   ENDIF
040600071003     C* CAD
040700070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
040800070928     C                   EXSR      CHKNUM
040900070928     C                   IF        PiInt=*on
041000070928     C                   Z-ADD     PiVal         Num5_0
041100070928     C                   MOVEL     Num5_0        VABCAD
041200070928     C                   ELSE
041300070928     C                   SETON                                        32
041400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041500070928     C                             + ' ' + 'VABCAD'
041600070928     C                   ENDIF
041700090112     C* RMN
041800090112     C                   EVAL      PiStr=%trim(%subst(vindta:190:7))
041900090112     C                   EXSR      CHKNUM
042000090112     C                   IF        PiInt=*on
042100090112     C                   Z-ADD     PiVal         VABRMN
042200090112     C***                Z-ADD     PiVal         VABNSP
042300090112     C***                Z-ADD     PiVal         VATNSP
042400090112     C                   ELSE
042500090112     C                   SETON                                        31
042600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700090112     C***                          + ' ' + 'VABNSP VATNSP VABRMN'
042800090112     C                             + ' ' + 'VABRMN'
042900070928     C                   ENDIF
043000071003     C* NCL
043100070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
043200070928     C                   EXSR      CHKNUM
043300070928     C                   IF        PiInt=*on
043400070928     C                   Z-ADD     PiVal         VABNCL
043500070928     C                   ELSE
043600070928     C                   SETON                                        32
043700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043800070928     C                             + ' ' + 'VABNCL'
043900070928     C                   ENDIF
044000071003     C* PKB
044100070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
044200070928     C                   EXSR      CHKNUM
044300070928     C                   IF        PiNum=*on
044400070928     C                   Z-ADD     PiVal         VABPKB
044500070928     C                   ELSE
044600070928     C                   SETON                                        32
044700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044800070928     C                             + ' ' + 'VABPKB'
044900070928     C                   ENDIF
045000071003     C* CAS
045100070928     C                   IF        %trim(%subst(vindta:280:12))<>*blanks AND
045200070928     C                             %trim(%subst(vindta:280:12))<>*zeros  AND
045300140513     C                             %trim(%subst(vindta:280:12))<>'0.00'  AND
045400140513     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
045500070928     C                   MOVEL     '1'           FlgCAS
045600070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
045700070928     C                   EXSR      CHKNUM
045800070928     C                   IF        PiNum=*on
045900071120     C                   EVAL      VABTIC = 'BM'
046000070928     C                   Z-ADD     PiVal         VABCAS
046100070928     C                   ELSE
046200070928     C                   SETON                                        32
046300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046400070928     C                             + ' ' + 'VABCAS'
046500070928     C                   ENDIF
046600070928     C                   ENDIF
046700071003     C* IAS
046800071003     C                   IF        %trim(%subst(vindta:295:12))<>*blanks AND
046900071003     C                             %trim(%subst(vindta:295:12))<>*zeros  AND
047000140513     C                             %trim(%subst(vindta:295:12))<>'0.00'  AND
047100140513     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
047200071003     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
047300071003     C                   EXSR      CHKNUM
047400071003     C                   IF        PiNum=*on
047500071003     C                   Z-ADD     PiVal         VABIAS
047600071003     C                   MOVEL     'EUR'         VABVAS
047700071003     C                   ELSE
047800071003     C                   SETON                                        32
047900071003     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000071003     C                             + ' ' + 'VABIAS'
048100071003     C                   ENDIF
048200071003     C                   ENDIF
048300070730     C*
048400070730     C* Considerazioni finali su CBO/CAS
048500070730     C                   IF        FlgCAS = '1'
048600070730     C                   IF        VABCBO = '1'
048700070730     C                   EVAL      VABCBO = '4'
048800070730     C                   ENDIF
048900070730     C                   IF        VABCBO = '2'
049000070730     C                   EVAL      VABCBO = '6'
049100070730     C                   ENDIF
049200070730     C                   ENDIF
049300020305     C*
049400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049500011113     C                   EXSR      CHKIMPDIV
049600010202     C*
049700000801     C* Ebbene...
049800000801     C                   ADD       1             §CTRMO
049900070530     C                   IF        *in31 <> *zeros OR
050000070530     C                             *in32 <> *zeros
050100000801     C                   ADD       1             §CTRNO
050200020725     C                   EVAL      x_vinflg = '2'
050300000801     C                   ELSE
050400010201     C                   ADD       1             §CTROKVB
050500000801     C                   ENDIF
050600000801     C*
050700000801     C                   ENDSR
050800010201     C*----------------------------------------------------*
050900021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051000010201     C*----------------------------------------------------*
051100020305     C     WRIVAT        BEGSR
051200010201     C*
051300021113     C* Valorizzo l buffer di scrittura del FIVAT
051400070928     C* - TIPO RECORD "B"
051500070928     C                   if        %subst(vindta:92:15) <> *blanks
051600070928     C                   eval      VATTRC = 'B'
051700070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
051800021113     C                   write     FIVAT000
051900020725     C                   endif
052000070928     C* - TIPO RECORD "E"
052100070928     C                   if        %subst(vindta:331:26) <> *blanks
052200070928     C                   eval      VATTRC = 'E'
052300070928     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
052400070928     C                   write     FIVAT000
052500070928     C                   endif
052600010201     C*
052700010201     C                   ENDSR
052800020904
052900020904
053000010202     C*----------------------------------------------------*
053100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053200010202     C*----------------------------------------------------*
053300020305     C     PREVAT        BEGSR
053400010202     C*
053500021113     C* Compongo il nome del membro da dare al FIVATWWR
053600010202     C                   eval      parmbr = vlrhdl
053700010202     C                   movel     'M'           parmbr
053800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053900010202     C                   eval      paropz = '1'
054000010202     C* Effettuo la chiamata al CLLE preposto
054100050128     C                   call(e)   'TITVVTC'
054200010202     C                   parm                    parccm
054300010202     C                   parm                    parmbr
054400010202     C                   parm                    paropz
054500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054600010202     C                   if        %error
054700010202     C                   movel     '1'           chkcall
054800010202     C                   else
054900010202     C                   movel     '0'           chkcall
055000010202     C                   endif
055100010202     C*
055200010202     C                   ENDSR
055300000801     C*----------------------------------------------------*
055400000801     C*  CONTROLLO NUMERICITA' CAMPI
055500000801     C*----------------------------------------------------*
055600000801     C     CHKNUM        BEGSR
055700000801     C*
055800000801     C                   call(e)   'ISNUMERIC'
055900000801     C                   PARM                    PiStr            30
056000140513     C                   PARM      '.'           PiDecChr          1
056100000801     C                   PARM      *ZEROS        PiVal            30 9
056200000801     C                   PARM      '0'           PiInt             1
056300000801     C                   PARM      '0'           PiNum             1
056400000801     C                   IF        %error
056500000801     C                   EVAL      PiInt=*off
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900000801     C***
057000000801
057100011113
057200011113     C*----------------------------------------------------*
057300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057400011113     C*----------------------------------------------------*
057500011113     C     CHKIMPDIV     BEGSR
057600011113     C*
057700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057800011113     C                   Z-ADD     *zeros        wrkDec            9 9
057900011113     C*
058000011113     C* Come prima cosa effettuo considerazioni sulla divisa
058100011113     C                   IF        vabIAS > *zeros
058200011113     C                   IF        vabVAS <> 'EUR'
058300011113     C                   EVAL      vabVAS =  'ITL'
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C                   IF        vabCAS > *zeros
058800011113     C                   IF        vabVCA <> 'EUR'
058900011113     C                   EVAL      vabVCA =  'ITL'
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C                   IF        vabVMD > *zeros
059400020305     C                   IF        vabVAD <> 'EUR'
059500011113     C                   EVAL      vabVAD =  'ITL'
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060000011113     C                   Z-ADD     vabIAS        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAS = 'ITL'
060300011113     C                   EVAL      vabIAS = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060800011113     C                   Z-ADD     vabCAS        wrkDec
060900011113     C                   IF        wrkDec > *zeros
061000011113     C                   IF        vabVCA = 'ITL'
061100011113     C                   EVAL      vabCAS = *zeros
061200011113     C                   ENDIF
061300011113     C                   ENDIF
061400011113     C*
061500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061600011113     C                   Z-ADD     vabVMD        wrkDec
061700011113     C                   IF        wrkDec > *zeros
061800011113     C                   IF        vabVAD = 'ITL'
061900011113     C                   EVAL      vabVMD = *zeros
062000011113     C                   ENDIF
062100011113     C                   ENDIF
062200011113     C*
062300011113     C                   ENDSR
062400011113     C***
062500011113
062600011113
062700000801
062800000801
062900990920      /TITLE Invio dei dati al punto operativo.
063000010202     C     invio         BEGSR
063100990920     C*
063200021113     C* 1° invio FIVAT
063300010201     C                   reset                   dscmz
063400010201     C                   move      vlrpoi        cmzdst
063500021113     C                   eval      cmzfld = 'FIVATWWR'
063600010201     C                   eval      cmzmbd = vlrhdl
063700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063800021009     C***                if        prmfir = *blanks
063900021113     C                   eval      cmzfla = 'FIVAT00F'
064000021113     C                   eval      cmzmba = 'FIVAT00F'
064100021009     C***                else
064200021009     C***                eval      cmzfla = prmfir
064300021009     C***                eval      cmzmba = prmfir
064400021009     C***                endif
064500010201     C                   eval      cmznrr = *zeros
064600020305     C                   move      §ctrokvt      cmznrr
064700021018     C                   eval      cmzlba = vlrfl1
064800010201     C                   call(e)   'TIS711C'
064900010201     C                   parm                    dscmz
065000010201     C                   parm      *blanks       esito
065100010205     C                   if        %error
065200010205     C                             or cmzerr = '1'
065300010205     C                             or esito  = '1'
065400010205     C                   eval      wrkesito = '3'
065500010205     C                   else
065600010201     C*
065700021113     C* 2° invio FIVAB
065800010201     C                   reset                   dscmz
065900010201     C                   move      vlrpoi        cmzdst
066000010201     C                   eval      cmzfld = vlrfou
066100010201     C                   eval      cmzmbd = vlrhdl
066200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066300021009     C***                if        prmfir = *blanks
066400021113     C                   eval      cmzfla = 'FIVAB00F'
066500021113     C                   eval      cmzmba = 'FIVAB00F'
066600021009     C***                else
066700021009     C***                eval      cmzfla = prmfir
066800021009     C***                eval      cmzmba = prmfir
066900021009     C***                endif
067000010201     C                   eval      cmznrr = *zeros
067100010201     C                   move      §ctrokvb      cmznrr
067200021018     C                   eval      cmzlba = vlrfl1
067300010201     C                   call(e)   'TIS711C'
067400010201     C                   parm                    dscmz
067500010201     C                   parm      *blanks       esito
067600010201     C                   if        %error
067700010201     C                             or cmzerr = '1'
067800010201     C                             or esito  = '1'
067900010201     C                   eval      wrkesito = '3'
068000010201     C                   endif
068100010205     C                   endif
068200990920     C*
068300000613     C                   ENDSR
068400000613     C***
068500070411
068600070411     C     *pssr         BEGSR
068700070411     C*
068800070411     C                   if        %open(tivin00r)
068900070411     C                   close     tivin00r
069000070411     C                   endif
069100070411     C                   if        %open(fivabwwr)
069200070411     C                   close     fivabwwr
069300070411     C                   endif
069400070411     C                   if        %open(fivatwwr)
069500070411     C                   close     fivatwwr
069600070411     C                   endif
069700070411     C*
069800070411     C* Effettuo la chiamata al CLLE preposto
069900070411     C                   call(e)   'TITVVTC'
070000070411     C                   parm                    parccm
070100070411     C                   parm                    parmbr
070200070411     C                   parm      '2'           paropz
070300070411     C*
070400070411     C                   eval      wrkesito = '2'
070500070411     C*
070600070411     C                   seton                                        LR
070700070411     C*
070800070411     C                   ENDSR     '*CANCL'
070900070411     C***
071000070411
071100990910
071200000613     C     *inzsr        BEGSR
071300990910     C*
071400990910     C     *entry        plist
071500990920     C                   parm                    tivlrds
071600990921     C                   parm      wrkesito      esito
071700000724     C                   parm                    prmlit
071800000710     C                   parm                    prmfir
071900000613     C*
072000000830     C* CALCOLA LA DATA CORRENTE
072100000830     C                   time                    wn14             14 0
072200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072300000830     C                   z-add     wn8           g08dat
072400000830     C                   z-add     *zeros        g08inv
072500000830     C                   movel     '0'           g08err
072600000830     C                   call      'XSRDA8'
072700000830     C                   parm                    wlbda8
072800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072900000830     C*
073000000613     C                   ENDSR
073100000613     C***
