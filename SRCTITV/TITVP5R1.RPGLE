000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700180219     FFIVADwwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900180308     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100041210     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500051206     D FlgNewBol       s              1    INZ(*blanks)
002600010201
002700010201
002800000913     C                   reset                   rrnum
002900990921     C                   reset                   esito
003000990921     C                   reset                   wrkesito
003100051228     C*
003200051228     C                   movel     '0'           wOVRPRTF          1
003300000613     C*
003400040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
003500000613     C*
003600010202     C* Effettuo la chiamata al CLLE preposto
003700180220     C                   SELECT
003800180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
003900180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
004000180220     C                   call(e)   'TITVVDTC'
004100180220     C                   parm                    parccm
004200180220     C                   parm                    parmbr
004300180220     C                   parm      '2'           paropz
004400180220     C                   OTHER
004500180220     C                   call(e)   'TITVVTC'
004600010202     C                   parm                    parccm
004700010202     C                   parm                    parmbr
004800010202     C                   parm      '2'           paropz
004900180220     C                   ENDSL
005000000616     C*
005100010201     C                   seton                                        LR
005200041210
005300041210
005400041210
005500910830     C*--------------------------------------------------------
005600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
005700910830     C*--------------------------------------------------------
005800040526     C     RWFILE        BEGSR
005900990910     C*
006000990914     C                   if        not %open(tivin00r)
006100990908     C                   open      tivin00r
006200990914     C                   endif
006300021113     C                   if        not %open(fivabwwr)
006400021113     C                   open      fivabwwr
006500990914     C                   endif
006600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
006700020305     C                   exsr      prevat
006800010201     C*
006900010202     C                   if        chkcall = '0'
007000010202     C*
007100021113     C                   if        not %open(fivatwwr)
007200021113     C                   open      fivatwwr
007300010201     C                   endif
007400180219     C*
007500180219     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
007600180219     C                   IF        %subst(vlrppt:16:2) = 'B2'
007700180219     C                   if        not %open(fivadwwr)
007800180219     C                   open      fivadwwr
007900180219     C                   endif
008000180219     C                   endif
008100990910     C*
008200010201     C                   clear                   §CTROKVB          5 0
008300020305     C                   clear                   §CTROKVT          5 0
008400000801     C                   clear                   §CTRMO            5 0
008500000801     C                   clear                   §CTRNO            5 0
008600040910     C*
008700921023     C                   DO        *HIVAL
008800990913     C*
008900990915     C                   READ      tivin00r                               70
009000040910     C                   if        vindta > *blanks
009100000613     C                   add       1             rrnum
009200000801     C*
009300000801     C                   if        *in70 = *off
009400000801     C                             and
009500000801     C                             (vinflg = *blanks
009600000801     C                              or vinflg = '0'
009700000801     C                              or vinflg = '2')
009800000801     C*
009900000801     C                   clear                   vinmsg
010000000801     C                   eval      vinflg = '1'
010100040910     C*
010200040910     C* Eseguo routine d traduzione
010300040910     C                   exsr      impvabvat
010400040802     C*
010500010305     C                   endif
010600000905     C*
010700000905     C                   else
010800000905     C                   eval      vinflg = '1'
010900000905     C                   endif
011000000905     C*
011100000905     C  N70              update    tivin000
011200000905     C*
011300991022     C  N70              ENDdo
011400041210     C*
011500050805     C* Scarico i buffer eventualmente rimasti in sospeso
011600041210     C                   IF        FlgNewBol = '1'
011700041210     C                   WRITE     FIVAB000
011800041210     C                   ENDIF
011900010202     C*
012000010202     C                   endif
012100990910
012200990910     C* Se non ci sono record con errori ...
012300000710     C                   if        §ctrno = 0
012400990910     C* ... restituisco esito OK.
012500990921     C                   eval      wrkesito = '0'
012600990910     C                   else
012700010201     C                   if        §ctrokvb > 0
012800990921     C                   eval      wrkesito = '1'
012900000710     C                   else
013000000710     C                   eval      wrkesito = '2'
013100990910     C                   endif
013200000710     C                   endif
013300990910     C*
013400990914     C                   if        %open(tivin00r)
013500990908     C                   close     tivin00r
013600990914     C                   endif
013700021113     C                   if        %open(fivabwwr)
013800021113     C                   close     fivabwwr
013900990914     C                   endif
014000021113     C                   if        %open(fivatwwr)
014100021113     C                   close     fivatwwr
014200010201     C                   endif
014300180219     C                   if        %open(fivadwwr)
014400180220     C                   close     fivadwwr
014500180219     C                   endif
014600990910     C*
014700010201     C                   if        §ctrokvb > 0
014800000724     C                             and vlrpoi <> *zeros
014900010202     C                   exsr      invio
015000990920     C                   endif
015100990920     C*
015200910830     C                   ENDSR
015300000613     C***
015400990920
015500000801     C*----------------------------------------------------*
015600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015700000801     C*----------------------------------------------------*
015800010201     C     INZVAR        BEGSR
015900000801     C*
016000040802     C                   Z-ADD     *zeros        Num5_0            5 0
016100051206     C                   MOVEL     '0'           FlgCAS            1
016200000801     C*
016300000801     C                   ENDSR
016400000801     C*----------------------------------------------------*
016500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016600000801     C*----------------------------------------------------*
016700000801     C     DEFCAM        BEGSR
016800180220     C*
016900021113     C                   CLEAR                   FIVAB000
017000040802     C                   CLEAR                   FIVAT000
017100180227     C                   CLEAR                   FIVAD000
017200051228     C* Imposto i valori di default...
017300051228     C                   Z-ADD     0053039       VABCCM
017400051228     C                   Z-ADD     0053039       VATCCM
017500051228     C                   Z-ADD     005           VABLNP
017600051228     C                   Z-ADD     005           VATLNP
017700090317     C                   Z-ADD     000           VABCTR
017800180220     C* Traffici IMPORT
017900051228     C   20              Z-ADD     0053578       VABCCM
018000051228     C   20              Z-ADD     0053578       VATCCM
018100051228     C   20              Z-ADD     005           VABLNP
018200051228     C   20              Z-ADD     005           VATLNP
018300051228     C   20              Z-ADD     000           VABCTR
018400051228     C   21              Z-ADD     0053039       VABCCM
018500051228     C   21              Z-ADD     0053039       VATCCM
018600051228     C   21              Z-ADD     005           VABLNP
018700051228     C   21              Z-ADD     005           VATLNP
018800090317     C   21              Z-ADD     000           VABCTR
018900060825     C*  22              Z-ADD     0493248       VABCCM
019000060825     C*  22              Z-ADD     0493248       VATCCM
019100060825     C   22              Z-ADD     1190493       VABCCM
019200060825     C   22              Z-ADD     1190493       VATCCM
019300060307     C   22              Z-ADD     119           VABLNP
019400060307     C   22              Z-ADD     119           VATLNP
019500051228     C   22              Z-ADD     001           VABCTR
019600100223 xxx C***23              Z-ADD     0650826       VABCCM
019700100223 xxx C***23              Z-ADD     0650826       VATCCM
019800100223     C   23              Z-ADD     0650945       VABCCM
019900100223     C   23              Z-ADD     0650945       VATCCM
020000051228     C   23              Z-ADD     065           VABLNP
020100051228     C   23              Z-ADD     065           VATLNP
020200051228     C   23              Z-ADD     001           VABCTR
020300051228     C   24              Z-ADD     0021744       VABCCM
020400051228     C   24              Z-ADD     0021744       VATCCM
020500051228     C   24              Z-ADD     002           VABLNP
020600051228     C   24              Z-ADD     002           VATLNP
020700051228     C   24              Z-ADD     001           VABCTR
020800120110     C   25              Z-ADD     2242028       VABCCM
020900120110     C   25              Z-ADD     2242028       VATCCM
021000120110     C   25              Z-ADD     224           VABLNP
021100120110     C   25              Z-ADD     224           VATLNP
021200051228     C   25              Z-ADD     001           VABCTR
021300051228     C   26              Z-ADD     0012821       VABCCM
021400051228     C   26              Z-ADD     0012821       VATCCM
021500051228     C   26              Z-ADD     001           VABLNP
021600051228     C   26              Z-ADD     001           VATLNP
021700051228     C   26              Z-ADD     001           VABCTR
021800051228     C   27              Z-ADD     1280515       VABCCM
021900051228     C   27              Z-ADD     1280515       VATCCM
022000051228     C   27              Z-ADD     128           VABLNP
022100051228     C   27              Z-ADD     128           VATLNP
022200051228     C   27              Z-ADD     001           VABCTR
022300051228     C* Traffici NAZIONALI
022400051228     C   28              Z-ADD     1280515       VABCCM
022500051228     C   28              Z-ADD     1280515       VATCCM
022600051228     C   28              Z-ADD     128           VABLNP
022700051228     C   28              Z-ADD     128           VATLNP
022800051228     C   28              Z-ADD     001           VABCTR
022900051228     C   29              Z-ADD     0502928       VABCCM
023000051228     C   29              Z-ADD     0502928       VATCCM
023100051228     C   29              Z-ADD     050           VABLNP
023200051228     C   29              Z-ADD     050           VATLNP
023300051228     C   29              Z-ADD     001           VABCTR
023400051228     C   30              Z-ADD     0012821       VABCCM
023500051228     C   30              Z-ADD     0012821       VATCCM
023600051228     C   30              Z-ADD     001           VABLNP
023700051228     C   30              Z-ADD     001           VATLNP
023800051228     C   30              Z-ADD     001           VABCTR
023900051228     C   31              Z-ADD     0939669       VABCCM
024000051228     C   31              Z-ADD     0939669       VATCCM
024100051228     C   31              Z-ADD     093           VABLNP
024200051228     C   31              Z-ADD     093           VATLNP
024300051228     C   31              Z-ADD     001           VABCTR
024400051228     C   32              Z-ADD     0840647       VABCCM
024500051228     C   32              Z-ADD     0840647       VATCCM
024600051228     C   32              Z-ADD     084           VABLNP
024700051228     C   32              Z-ADD     084           VATLNP
024800051228     C   32              Z-ADD     001           VABCTR
024900051228     C   33              Z-ADD     0021744       VABCCM
025000051228     C   33              Z-ADD     0021744       VATCCM
025100051228     C   33              Z-ADD     002           VABLNP
025200051228     C   33              Z-ADD     002           VATLNP
025300051228     C   33              Z-ADD     001           VABCTR
025400100223     C***34              Z-ADD     0650826       VABCCM
025500100223     C***34              Z-ADD     0650826       VATCCM
025600120608     C***34              Z-ADD     0202104       VABCCM
025700120608     C***34              Z-ADD     0202104       VATCCM
025800120608     C   34              Z-ADD     0203067       VABCCM
025900120608     C   34              Z-ADD     0203067       VATCCM
026000100223     C   34              Z-ADD     020           VABLNP
026100100223     C   34              Z-ADD     020           VATLNP
026200051228     C   34              Z-ADD     001           VABCTR
026300051228     C   35              Z-ADD     0063055       VABCCM
026400051228     C   35              Z-ADD     0063055       VATCCM
026500051228     C   35              Z-ADD     006           VABLNP
026600051228     C   35              Z-ADD     006           VATLNP
026700051228     C   35              Z-ADD     001           VABCTR
026800051228     C   36              Z-ADD     0201370       VABCCM
026900051228     C   36              Z-ADD     0201370       VATCCM
027000051228     C   36              Z-ADD     020           VABLNP
027100051228     C   36              Z-ADD     020           VATLNP
027200051228     C   36              Z-ADD     001           VABCTR
027300060825     C*  37              Z-ADD     0493248       VABCCM
027400060825     C*  37              Z-ADD     0493248       VATCCM
027500060825     C   37              Z-ADD     1190493       VABCCM
027600060825     C   37              Z-ADD     1190493       VATCCM
027700060307     C   37              Z-ADD     119           VABLNP
027800060307     C   37              Z-ADD     119           VATLNP
027900051228     C   37              Z-ADD     001           VABCTR
028000051228     C   38              Z-ADD     0433239       VABCCM
028100051228     C   38              Z-ADD     0433239       VATCCM
028200051228     C   38              Z-ADD     043           VABLNP
028300051228     C   38              Z-ADD     043           VATLNP
028400051228     C   38              Z-ADD     001           VABCTR
028500101012     C   39              Z-ADD     1110743       VABCCM
028600101012     C   39              Z-ADD     1110743       VATCCM
028700110221     C   39              Z-ADD     166           VABLNP
028800110221     C   39              Z-ADD     166           VATLNP
028900101012     C   39              Z-ADD     001           VABCTR
029000120213     C*
029100120213     C   40              Z-ADD     0021744       VABCCM
029200120213     C   40              Z-ADD     0021744       VATCCM
029300120213     C   40              Z-ADD     002           VABLNP
029400120213     C   40              Z-ADD     002           VATLNP
029500120213     C   40              Z-ADD     001           VABCTR
029600120213     C   41              Z-ADD     0220905       VABCCM
029700120213     C   41              Z-ADD     0220905       VATCCM
029800120213     C   41              Z-ADD     022           VABLNP
029900120213     C   41              Z-ADD     022           VATLNP
030000120213     C   41              Z-ADD     001           VABCTR
030100120213     C   42              Z-ADD     0939669       VABCCM
030200120213     C   42              Z-ADD     0939669       VATCCM
030300120213     C   42              Z-ADD     093           VABLNP
030400120213     C   42              Z-ADD     093           VATLNP
030500120213     C   42              Z-ADD     001           VABCTR
030600120213     C   43              Z-ADD     0471329       VABCCM
030700120213     C   43              Z-ADD     0471329       VATCCM
030800120213     C   43              Z-ADD     047           VABLNP
030900120213     C   43              Z-ADD     047           VATLNP
031000120213     C   43              Z-ADD     001           VABCTR
031100120213     C   44              Z-ADD     0063055       VABCCM
031200120213     C   44              Z-ADD     0063055       VATCCM
031300120213     C   44              Z-ADD     006           VABLNP
031400120213     C   44              Z-ADD     006           VATLNP
031500120213     C   44              Z-ADD     001           VABCTR
031600120213     C   45              Z-ADD     0982240       VABCCM
031700120213     C   45              Z-ADD     0982240       VATCCM
031800120213     C   45              Z-ADD     098           VABLNP
031900120213     C   45              Z-ADD     098           VATLNP
032000120213     C   45              Z-ADD     001           VABCTR
032100120213     C   46              Z-ADD     0400290       VABCCM
032200120213     C   46              Z-ADD     0400290       VATCCM
032300120213     C   46              Z-ADD     040           VABLNP
032400120213     C   46              Z-ADD     040           VATLNP
032500120213     C   46              Z-ADD     001           VABCTR
032600120213     C   47              Z-ADD     0250618       VABCCM
032700120213     C   47              Z-ADD     0250618       VATCCM
032800120213     C   47              Z-ADD     025           VABLNP
032900120213     C   47              Z-ADD     025           VATLNP
033000120213     C   47              Z-ADD     001           VABCTR
033100120213     C   48              Z-ADD     2242028       VABCCM
033200120213     C   48              Z-ADD     2242028       VATCCM
033300120213     C   48              Z-ADD     224           VABLNP
033400120213     C   48              Z-ADD     224           VATLNP
033500120213     C   48              Z-ADD     001           VABCTR
033600120213     C   49              Z-ADD     0661774       VABCCM
033700120213     C   49              Z-ADD     0661774       VATCCM
033800120213     C   49              Z-ADD     066           VABLNP
033900120213     C   49              Z-ADD     066           VATLNP
034000120213     C   49              Z-ADD     001           VABCTR
034100120213     C   50              Z-ADD     1230516       VABCCM
034200120213     C   50              Z-ADD     1230516       VATCCM
034300120213     C   50              Z-ADD     123           VABLNP
034400120213     C   50              Z-ADD     123           VATLNP
034500120213     C   50              Z-ADD     001           VABCTR
034600120720     C   51              Z-ADD     0203097       VABCCM
034700120720     C   51              Z-ADD     0203097       VATCCM
034800120926     C***51              Z-ADD     223           VABLNP
034900120926     C***51              Z-ADD     223           VATLNP
035000120926     C   51              Z-ADD     020           VABLNP
035100120926     C   51              Z-ADD     020           VATLNP
035200120720     C   51              Z-ADD     000           VABCTR
035300121030     C   52              Z-ADD     0952024       VABCCM
035400121030     C   52              Z-ADD     0952024       VATCCM
035500121030     C   52              Z-ADD     095           VABLNP
035600121030     C   52              Z-ADD     095           VATLNP
035700121030     C   52              Z-ADD     001           VABCTR
035800180308     c* Nuovo traffico dalla 166
035900180308     C   53              Z-ADD     1664129       VABCCM
036000180308     C   53              Z-ADD     1664129       VATCCM
036100180308     C   53              Z-ADD     166           VABLNP
036200180308     C   53              Z-ADD     166           VATLNP
036300180308     C   53              Z-ADD     399           VABCTR
036400051228     C*
036500051206     C                   MOVEL     '7Q'          VABCTM
036600051206     C                   MOVEL     '1'           VABCBO
036700020619     C* ... e poi verifico se sono stati passati come parametri
036800020619     C                   IF        vlrppt > *blanks
036900040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
037000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037100020619     C                   EXSR      CHKNUM
037200020619     C                   IF        PiInt=*on
037300020619     C                   Z-ADD     PiVal         VABCCM
037400020619     C                   Z-ADD     PiVal         VATCCM
037500180220     C                   Z-ADD     PiVal         VADCCM
037600020619     C                   ENDIF
037700040506     C                   ENDIF
037800040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
037900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038000020619     C                   EXSR      CHKNUM
038100020619     C                   IF        PiInt=*on
038200020619     C                   Z-ADD     PiVal         VABLNP
038300020619     C                   Z-ADD     PiVal         VATLNP
038400180220     C                   Z-ADD     PiVal         VADLNP
038500040506     C                   ENDIF
038600020619     C                   ENDIF
038700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
038800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038900020619     C                   EXSR      CHKNUM
039000020619     C                   IF        PiInt=*on
039100020619     C                   Z-ADD     PiVal         VABCTR
039200040506     C                   ENDIF
039300020619     C                   ENDIF
039400041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
039500041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
039600041210     C                   ENDIF
039700020619     C                   ENDIF
039800000801     C*
039900000801     C                   ENDSR
040000000801     C*----------------------------------------------------*
040100040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
040200000801     C*----------------------------------------------------*
040300040910     C     IMPVABVAT     BEGSR
040400041210     C*
040500041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
040600041210     C                   Z-ADD     *zeros        errore            1 0
040700051213     C*
040800051213     C********
040900051213     C* Tipo record 'VIA' (riconoscimento cliente mittente)
041000051213     C********
041100051213     C                   IF        %trim(%subst(vindta:1:3)) = 'VIA'
041200051228     C                   SETOFF                                       202122
041300051228     C                   SETOFF                                       232425
041400051228     C                   SETOFF                                       262728
041500051228     C                   SETOFF                                       293031
041600051228     C                   SETOFF                                       323334
041700051228     C                   SETOFF                                       353637
041800101012     C                   SETOFF                                       3839
041900120213     C                   SETOFF                                       404142
042000120213     C                   SETOFF                                       434445
042100120213     C                   SETOFF                                       464748
042200120720     C                   SETOFF                                       495051
042300180308     C                   SETOFF                                       5253
042400080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
042500051228     C                             %trim(%subst(vindta:116:5)) = 'BOSCH'
042600051228     C                   SETON                                        20
042700051213     C                   ENDIF
042800080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
042900051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043000051228     C                   SETON                                        21
043100051228     C                   ENDIF
043200080109     C                   IF        %trim(%subst(vindta:54:2)) = '17' AND
043300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043400051228     C                   SETON                                        22
043500051228     C                   ENDIF
043600080109     C                   IF        %trim(%subst(vindta:54:2)) = '11' AND
043700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043800051228     C                   SETON                                        23
043900051228     C                   ENDIF
044000080109     C                   IF        %trim(%subst(vindta:54:2)) = '10' AND
044100051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044200051228     C                   SETON                                        24
044300051228     C                   ENDIF
044400080109     C                   IF        %trim(%subst(vindta:54:2)) = '08' AND
044500051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044600051228     C                   SETON                                        25
044700051228     C                   ENDIF
044800080109     C                   IF        %trim(%subst(vindta:54:2)) = '04' AND
044900051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045000051228     C                   SETON                                        26
045100051228     C                   ENDIF
045200080109     C                   IF        %trim(%subst(vindta:54:2)) = '02' AND
045300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045400051228     C                   SETON                                        27
045500051228     C                   ENDIF
045600080109     C                   IF        %trim(%subst(vindta:54:2)) = '03'      AND
045700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045800051228     C                   SETON                                        29
045900051228     C                   ENDIF
046000080109     C                   IF        %trim(%subst(vindta:54:2)) = '05'      AND
046100051228     C                             %trim(%subst(vindta:116:5)) = *blanks
046200051228     C                   SETON                                        31
046300051228     C                   ENDIF
046400100223     C                   IF        %trim(%subst(vindta:54:2)) = '34'      AND
046500100223     C                             %trim(%subst(vindta:116:5)) = *blanks
046600100223     C                   SETON                                        34
046700100223     C                   ENDIF
046800080109     C                   IF        %trim(%subst(vindta:54:2)) = '12'      AND
046900051228     C                             %trim(%subst(vindta:116:5)) = *blanks
047000051228     C                   SETON                                        35
047100051228     C                   ENDIF
047200080109     C                   IF        %trim(%subst(vindta:54:2)) = '13'      AND
047300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
047400051228     C                   SETON                                        36
047500051228     C                   ENDIF
047600080109     C                   IF        %trim(%subst(vindta:54:2)) = '18'      AND
047700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
047800051228     C                   SETON                                        38
047900051228     C                   ENDIF
048000110607     C***                IF        %trim(%subst(vindta:54:2)) = '06'
048100110607     C                   IF        %trim(%subst(vindta:54:2)) = '75'
048200101012     C                   SETON                                        39
048300101012     C                   ENDIF
048400120213     C                   IF        %trim(%subst(vindta:54:2)) = '10'
048500120213     C                   SETON                                        40
048600120213     C                   ENDIF
048700120213     C                   IF        %trim(%subst(vindta:54:2)) = '36'
048800120213     C                   SETON                                        41
048900120213     C                   ENDIF
049000120213     C                   IF        %trim(%subst(vindta:54:2)) = '05'
049100120213     C                   SETON                                        42
049200120213     C                   ENDIF
049300120213     C                   IF        %trim(%subst(vindta:54:2)) = '72'
049400120213     C                   SETON                                        43
049500120213     C                   ENDIF
049600120213     C                   IF        %trim(%subst(vindta:54:2)) = '38'
049700120213     C                   SETON                                        44
049800120213     C                   ENDIF
049900120213     C                   IF        %trim(%subst(vindta:54:2)) = '20'
050000120213     C                   SETON                                        45
050100120213     C                   ENDIF
050200120213     C                   IF        %trim(%subst(vindta:54:2)) = '23'
050300120213     C                   SETON                                        46
050400120213     C                   ENDIF
050500120213     C                   IF        %trim(%subst(vindta:54:2)) = '09'
050600120213     C                   SETON                                        47
050700120213     C                   ENDIF
050800120213     C                   IF        %trim(%subst(vindta:54:2)) = '08'
050900120213     C                   SETON                                        48
051000120213     C                   ENDIF
051100120213     C                   IF        %trim(%subst(vindta:54:2)) = '02'
051200120213     C                   SETON                                        49
051300120213     C                   ENDIF
051400120213     C                   IF        %trim(%subst(vindta:54:2)) = '15'
051500120213     C                   SETON                                        50
051600120213     C                   ENDIF
051700120720     C                   IF        %trim(%subst(vindta:54:2)) = '36'
051800120720     C                   SETON                                        51
051900120720     C                   ENDIF
052000121030     C                   IF        %trim(%subst(vindta:54:2)) = '37'
052100121030     C                   SETON                                        52
052200121030     C                   ENDIF
052300180308     c* nuove codice cliente della 166
052400180308     C                   IF        %trim(%subst(vindta:54:2)) = '14'
052500180308     C                   SETON                                        53
052600180308     C                   ENDIF
052700180308
052800101012     C                   ENDIF
052900051213     C********
053000051213     C* FINE Tipo record 'VIA'
053100051213     C********
053200040910     C*
053300041210     C********
053400051206     C* Tipo record 'PR1' (riferimenti, dati destinatario, importi e particolarità consegna)
053500041210     C********
053600051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR1'
053700041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
053800041210     C                   IF        FlgNewBol = '1'
053900041210     C                   WRITE     FIVAB000
054000041210     C                   EVAL      FlgNewBol = '0'
054100041210     C                   ENDIF
054200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
054300040910     C                   EXSR      INZVAR
054400040910     C                   EXSR      DEFCAM
054500041210     C                   EVAL      FlgNewBol = '1'
054600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
054700040928     C                   MOVEL     datcor        VABAAS
054800040928     C                   MOVEL     datcor        VATAAS
054900040928     C                   MOVE      datcor        VABMGS
055000051228     C                   MOVE(P)   VABLNP        VABFGS
055100051228     C                   MOVE(P)   VABLNP        VATFGS
055200180219     C                   MOVE(P)   VABLNP        VADFGS
055300180219     C                   MOVEL     datcor        VADAAS
055400180227     C*
055500180227     C* Dati CHIAVE esclusivi SOLO per alcuni clienti
055600180227     C                   SELECT
055700180227     C                   WHEN      VABCCM = 2230606
055800180227     C* ......VABNRS
055900180227     C                   EVAL      PiStr=%trim(%subst(vindta:873:2))
056000180227     C                   EXSR      CHKNUM
056100180227     C                   IF        PiInt=*on
056200180227     C                   Z-ADD     PiVal         VABNRS
056300180227     C                   Z-ADD     PiVal         VATNRS
056400180227     C                   Z-ADD     PiVal         VADNRS
056500180227     C                   ELSE
056600180227     C                   ADD       1             errore
056700180227     C                   EVAL      vinmsg = %trimr(vinmsg)
056800180227     C                             + ' ' + 'VABNRS VATNRS VADNRS'
056900180227     C                   ENDIF
057000180227     C*
057100180227     C                   ENDSL
057200180227     C*
057300051206     C* ......VABNSP/VATNSP/VABRMN/VABRMA
057400051206     C                   EVAL      VABRMA=%trim(%subst(vindta:4:27))
057500051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
057600051206     C                   EXSR      CHKNUM
057700051206     C                   IF        PiInt=*on
057800051206     C                   Z-ADD     PiVal         VABRMN
057900051206     C                   ELSE
058000051206     C                   ADD       1             errore
058100051206     C                   EVAL      vinmsg = %trimr(vinmsg)
058200051206     C                             + ' ' + 'VABRMN'
058300051206     C                   ENDIF
058400051206     C*
058500051206     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
058600051206     C                   EXSR      CHKNUM
058700051206     C                   IF        PiInt=*on
058800051206     C                   Z-ADD     PiVal         VABNSP
058900051206     C                   Z-ADD     PiVal         VATNSP
059000180219     C                   Z-ADD     PiVal         VADNSP
059100041210     C                   ELSE
059200041210     C                   ADD       1             errore
059300041210     C                   EVAL      vinmsg = %trimr(vinmsg)
059400180219     C                             + ' ' + 'VABNSP VATNSP'
059500041210     C                   ENDIF
059600051206     C* ......VABRMO
059700051206     C                   IF        %trim(%subst(vindta:46:35)) <>
059800051206     C                             'GEODIS ZUST AMBROSETTI'
059900051206     C                   EVAL      VABRMO=%trim(%subst(vindta:46:35))
060000051206     C                   ENDIF
060100051206     C* ......VATNOT + VATTRC = 'B'
060200051206     C                   IF        %trim(%subst(vindta:272:35)) <> *blanks
060300051206     C                   EVAL      VATNOT=%trim(%subst(vindta:272:35))
060400051206     C                   EVAL      VATTRC = 'B'
060500051206     C                   WRITE     FIVAT000
060600051206     C                   ENDIF
060700051206     C* ......VABRSD
060800051206     C                   EVAL      VABRSD=%trim(%subst(vindta:428:35))
060900050805     C* ......VABIND
061000051206     C                   EVAL      VABIND=%trim(%subst(vindta:498:35))
061100050805     C* ......VABLOD
061200051206     C                   EVAL      VABLOD=%trim(%subst(vindta:578:35))
061300050805     C* ......VABCAD
061400051206     C                   EVAL      PiStr=%trim(%subst(vindta:568:10))
061500050805     C                   EXSR      CHKNUM
061600050805     C                   IF        PiInt=*on
061700050805     C                   Z-ADD     PiVal         Num5_0
061800050805     C                   MOVEL(P)  Num5_0        VABCAD
061900050805     C                   ELSE
062000050805     C                   ADD       1             errore
062100050805     C                   EVAL      vinmsg = %trimr(vinmsg)
062200050805     C                             + ' ' + 'VABCAD'
062300050805     C                   ENDIF
062400051206     C* ......VABPRD
062500051206     C                   EVAL      VABPRD=%trim(%subst(vindta:613:3))
062600051206     C* ......VABCBO
062700051206     C                   IF        %trim(%subst(vindta:619:3)) = 'DDU'
062800051206     C                   EVAL      VABCBO='1'
062900051206     C                   ENDIF
063000051206     C                   IF        %trim(%subst(vindta:619:3)) = 'EXW'
063100051206     C                   EVAL      VABCBO='2'
063200051206     C                   ENDIF
063300051206     C* ......VABCAS/VABVCA
063400051206     C                   IF        %trim(%subst(vindta:628:15)) <> *zeros
063500051206     C                   EVAL      FlgCAS = '1'
063600051206     C*
063700051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBK'
063800051206     C                   EVAL      VABTIC='OM'
063900051206     C                   ENDIF
064000051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBC'
064100051206     C                   EVAL      VABTIC='OM'
064200051206     C                   ENDIF
064300051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YCB'
064400051206     C                   EVAL      VABTIC='OC'
064500051206     C                   ENDIF
064600051206     C                   IF        %trim(%subst(vindta:643:3)) = 'CSH'
064700051206     C                   EVAL      VABTIC=*blanks
064800051206     C                   ENDIF
064900051206     C*
065000051206     C                   EVAL      VABVCA=%trim(%subst(vindta:625:3))
065100051206     C*
065200051206     C                   EVAL      PiStr=%trim(%subst(vindta:628:15))
065300051206     C                   EXSR      CHKNUM
065400051206     C                   IF        PiNum=*on
065500051206     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
065600051206     C                   Z-ADD     PiVal         VABCAS
065700051206     C                   ELSE
065800051206     C                   ADD       1             errore
065900051206     C                   EVAL      vinmsg = %trimr(vinmsg)
066000051206     C                             + ' ' + 'VABCAS'
066100051206     C                   ENDIF
066200051206     C                   ENDIF
066300180227     C*
066400180227     C* Dati NON CHIAVE esclusivi SOLO per alcuni clienti
066500180227     C                   SELECT
066600180227     C                   WHEN      VABCCM = 2230606
066700180227     C* ......VABLNA
066800180227     C                   EVAL      PiStr=%trim(%subst(vindta:875:3))
066900180227     C                   EXSR      CHKNUM
067000180227     C                   IF        PiInt=*on
067100180227     C                   Z-ADD     PiVal         VABLNA
067200180227     C                   ELSE
067300180227     C                   ADD       1             errore
067400180227     C                   EVAL      vinmsg = %trimr(vinmsg)
067500180227     C                             + ' ' + 'VABLNA'
067600180227     C                   ENDIF
067700180227     C* ......VABZNC
067800180227     C                   EVAL      PiStr=%trim(%subst(vindta:878:2))
067900180227     C                   EXSR      CHKNUM
068000180227     C                   IF        PiInt=*on
068100180227     C                   Z-ADD     PiVal         VABZNC
068200180227     C                   ELSE
068300180227     C                   ADD       1             errore
068400180227     C                   EVAL      vinmsg = %trimr(vinmsg)
068500180227     C                             + ' ' + 'VABZNC'
068600180227     C                   ENDIF
068700180227     C* ......VATNOT + VATTRC = 'P'
068800180227     C                   IF        %trim(%subst(vindta:856:17)) <> *blanks
068900180227     C                   EVAL      VATNOT=%trim(%subst(vindta:856:17))
069000180227     C                   EVAL      %subst(VATNOT:34:2)='10'
069100180227     C                   EVAL      VATTRC = 'P'
069200180227     C                   WRITE     FIVAT000
069300180227     C                   ENDIF
069400180227     C*
069500180227     C                   ENDSL
069600180219     C*
069700060307     C* Considerazioni sul contenuto di campi precedentemente valorizzati
069800060307     C                   IF        FlgCAS <> '0'
069900060307     C                   IF        VABCBO = '1'
070000060307     C                   EVAL      VABCBO = '4'
070100060307     C                   ELSE
070200060307     C                   EVAL      VABCBO = '6'
070300060307     C                   ENDIF
070400060307     C                   ENDIF
070500051206     C*
070600051206     C                   ENDIF
070700051206     C********
070800051206     C* FINE Tipo record 'PR1'
070900051206     C********
071000051206     C*
071100051206     C********
071200051206     C* Tipo record 'PR2' (note, consegna richiesta, valore da assicurare)
071300051206     C********
071400051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR2'
071500051206     C* ......VABDCR
071600051213     C                   IF        %subst(vindta:7:8)<>*blanks
071700051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
071800050805     C                   EXSR      CHKNUM
071900051206     C                   IF        PiInt=*on
072000051206     C                   Z-ADD     PiVal         VABDCR
072100050805     C                   ELSE
072200050805     C                   ADD       1             errore
072300050805     C                   EVAL      vinmsg = %trimr(vinmsg)
072400051206     C                             + ' ' + 'VABDCR'
072500050805     C                   ENDIF
072600051213     C                   ENDIF
072700051206     C* ......VABHCR
072800051213     C                   IF        %subst(vindta:7+8:4)<>*blanks
072900051206     C                   EVAL      PiStr=%trim(%subst(vindta:7+8:4))
073000051206     C                   EXSR      CHKNUM
073100051206     C                   IF        PiInt=*on
073200051206     C                   Z-ADD     PiVal         VABHCR
073300051206     C                   ELSE
073400051206     C                   ADD       1             errore
073500051206     C                   EVAL      vinmsg = %trimr(vinmsg)
073600051206     C                             + ' ' + 'VABHCR'
073700051206     C                   ENDIF
073800051213     C                   ENDIF
073900051206     C* ......VABNOT
074000051206     C                   EVAL      VABNOT=%trim(%subst(vindta:22:35))
074100051206     C* ......VABNT2
074200051206     C                   EVAL      VABNT2=%trim(%subst(vindta:22+35:35))
074300051206     C* ......VABIAS/VABVAS
074400051206     C                   IF        %subst(vindta:845:1) = 'S'
074500051206     C                   EVAL      VABVAS=%trim(%subst(vindta:846:3))
074600051206     C                   EVAL      PiStr=%trim(%subst(vindta:849:15))
074700051206     C                   EXSR      CHKNUM
074800051206     C                   IF        PiNum=*on
074900051206     C                   EVAL      PiVal = PiVal / 100
075000051206     C                   Z-ADD     PiVal         VABIAS
075100051206     C                   ELSE
075200051206     C                   ADD       1             errore
075300051206     C                   EVAL      vinmsg = %trimr(vinmsg)
075400051206     C                             + ' ' + 'VABIAS'
075500051206     C                   ENDIF
075600051206     C                   ENDIF
075700051206     C*
075800051206     C                   ENDIF
075900051206     C********
076000051206     C* FINE Tipo record 'PR2'
076100051206     C********
076200051206     C*
076300051206     C********
076400051206     C* Tipo record 'MER' (colli, peso)
076500051206     C********
076600051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MER'
076700050805     C* ......VABNCL
076800051206     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
076900050805     C                   EXSR      CHKNUM
077000050805     C                   IF        PiInt=*on
077100080208     C***                Z-ADD     PiVal         VABNCL
077200080208     C                   ADD       PiVal         VABNCL
077300050805     C                   ELSE
077400050805     C                   ADD       1             errore
077500050805     C                   EVAL      vinmsg = %trimr(vinmsg)
077600050805     C                             + ' ' + 'VABNCL'
077700050805     C                   ENDIF
077800050805     C* ......VABPKB
077900051206     C                   EVAL      PiStr=%trim(%subst(vindta:15:10))
078000050805     C                   EXSR      CHKNUM
078100050805     C                   IF        PiNum=*on
078200051206     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
078300080208     C***                Z-ADD     PiVal         VABPKB
078400080208     C                   ADD       PiVal         VABPKB
078500050805     C                   ELSE
078600050805     C                   ADD       1             errore
078700050805     C                   EVAL      vinmsg = %trimr(vinmsg)
078800050805     C                             + ' ' + 'VABPKB'
078900050805     C                   ENDIF
079000061012     C* ......VABVLB
079100061012     C                   EVAL      PiStr=%trim(%subst(vindta:35:10))
079200061012     C                   EXSR      CHKNUM
079300061012     C                   IF        PiNum=*on
079400061012     C                   EVAL      PiVal = PiVal / 1000                         * da decim3 a metri3
079500080208     C***                Z-ADD     PiVal         VABVLB
079600080208     C                   ADD       PiVal         VABVLB
079700061012     C                   ELSE
079800061012     C                   ADD       1             errore
079900061012     C                   EVAL      vinmsg = %trimr(vinmsg)
080000061012     C                             + ' ' + 'VABVLB'
080100061012     C                   ENDIF
080200050805     C*
080300050805     C                   ENDIF
080400051206     C********
080500051206     C* FINE Tipo record 'MER'
080600051206     C********
080700051206     C*
080800051206     C********
080900051206     C* Tipo record 'MAR' (barcode)
081000051206     C********
081100051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MAR'
081200180219     C* e non è il tipo rcd MAR24 che non ha il barcode ma il collo
081300180219     C                             and %trim(%subst(vindta:1:5)) <>'MAR24'
081400051206     C* ......VATNOT + VATTRC = 'E'
081500051213     C                   IF        %trim(%subst(vindta:7:20)) <> *blanks
081600051213     C                   EVAL      VATNOT=%trim(%subst(vindta:7:20))
081700051206     C                   EVAL      VATTRC = 'E'
081800051206     C                   WRITE     FIVAT000
081900051206     C                   ENDIF
082000050805     C*
082100050805     C                   ENDIF
082200051206     C********
082300051206     C* FINE Tipo record 'MAR'
082400051206     C********
082500180219     C*
082600180219     C********
082700180219     C* Tipo record 'MAR24' (marcatura alternativa)
082800180219     C********
082900180219     C                   IF        %trim(%subst(vindta:1:5)) = 'MAR24'
083000180219     C* devo essere in sol.B2
083100180219     C                             and %subst(vlrppt:16:2) = 'B2'
083200180219     C* ......VADNCD
083300180219     C                   IF        %trim(%subst(vindta:50:7)) <> *blanks
083400180219     C                   EVAL      PiStr=%trim(%subst(vindta:50:7))
083500180219     C                   EXSR      CHKNUM
083600180219     C                   IF        PiInt=*on
083700180219     C                   Z-ADD     PiVal         VADNCD
083800180227     C                   Z-ADD     1             VADNCL
083900180220     C                   WRITE     FIVAD000
084000180219     C                   ELSE
084100180219     C                   ADD       1             errore
084200180219     C                   EVAL      vinmsg = %trimr(vinmsg)
084300180219     C                             + ' ' + 'VADNCD'
084400180219     C                   ENDIF
084500180219     C                   ENDIF
084600180219     C*
084700180219     C                   ENDIF
084800180219     C********
084900180219     C* FINE Tipo record 'MAR'
085000180219     C********
085100040910     C*
085200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
085300040910     C                   EXSR      CHKIMPDIV
085400010202     C*
085500000801     C* Ebbene...
085600000801     C                   ADD       1             §CTRMO
085700010201     C                   IF        errore <> *zeros
085800000801     C                   ADD       1             §CTRNO
085900000801     C                   EVAL      vinflg = '2'
086000000801     C                   ELSE
086100010201     C                   ADD       1             §CTROKVB
086200000801     C                   ENDIF
086300000801     C*
086400000801     C                   ENDSR
086500010202     C*----------------------------------------------------*
086600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
086700010202     C*----------------------------------------------------*
086800020305     C     PREVAT        BEGSR
086900010202     C*
087000021113     C* Compongo il nome del membro da dare al FIVATWWR
087100010202     C                   eval      parmbr = vlrhdl
087200010202     C                   movel     'M'           parmbr
087300041210     C                   eval      parccm = vlrksc
087400010202     C                   eval      paropz = '1'
087500010202     C* Effettuo la chiamata al CLLE preposto
087600180220     C                   SELECT
087700180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
087800180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
087900180220     C                   call(e)   'TITVVDTC'
088000180220     C                   parm                    parccm
088100180220     C                   parm                    parmbr
088200180220     C                   parm                    paropz
088300180220     C                   OTHER
088400180220     C                   call(e)   'TITVVTC'
088500180220     C                   parm                    parccm
088600180220     C                   parm                    parmbr
088700180220     C                   parm                    paropz
088800180220     C                   ENDSL
088900180220     C*
089000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
089100010202     C                   if        %error
089200010202     C                   movel     '1'           chkcall
089300010202     C                   else
089400010202     C                   movel     '0'           chkcall
089500010202     C                   endif
089600010202     C*
089700010202     C                   ENDSR
089800000801     C*----------------------------------------------------*
089900000801     C*  CONTROLLO NUMERICITA' CAMPI
090000000801     C*----------------------------------------------------*
090100000801     C     CHKNUM        BEGSR
090200000801     C*
090300000801     C                   call(e)   'ISNUMERIC'
090400000801     C                   PARM                    PiStr            30
090500041210     C                   PARM      '.'           PiDecChr          1
090600000801     C                   PARM      *ZEROS        PiVal            30 9
090700000801     C                   PARM      '0'           PiInt             1
090800000801     C                   PARM      '0'           PiNum             1
090900000801     C                   IF        %error
091000000801     C                   EVAL      PiInt=*off
091100000801     C                   ENDIF
091200000801     C*
091300000801     C                   ENDSR
091400000801     C***
091500000801
091600011113
091700011113     C*----------------------------------------------------*
091800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
091900011113     C*----------------------------------------------------*
092000011113     C     CHKIMPDIV     BEGSR
092100011113     C*
092200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
092300011113     C                   Z-ADD     *zeros        wrkDec            9 9
092400011113     C*
092500011113     C* Come prima cosa effettuo considerazioni sulla divisa
092600011113     C                   IF        vabIAS > *zeros
092700011113     C                   IF        vabVAS <> 'EUR'
092800011113     C                   EVAL      vabVAS =  'ITL'
092900011113     C                   ENDIF
093000011113     C                   ENDIF
093100011113     C*
093200011113     C                   IF        vabCAS > *zeros
093300011113     C                   IF        vabVCA <> 'EUR'
093400011113     C                   EVAL      vabVCA =  'ITL'
093500011113     C                   ENDIF
093600011113     C                   ENDIF
093700011113     C*
093800011113     C                   IF        vabVMD > *zeros
093900020305     C                   IF        vabVAD <> 'EUR'
094000011113     C                   EVAL      vabVAD =  'ITL'
094100011113     C                   ENDIF
094200011113     C                   ENDIF
094300011113     C*
094400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
094500011113     C                   Z-ADD     vabIAS        wrkDec
094600011113     C                   IF        wrkDec > *zeros
094700011113     C                   IF        vabVAS = 'ITL'
094800011113     C                   EVAL      vabIAS = *zeros
094900011113     C                   ENDIF
095000011113     C                   ENDIF
095100011113     C*
095200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
095300011113     C                   Z-ADD     vabCAS        wrkDec
095400011113     C                   IF        wrkDec > *zeros
095500011113     C                   IF        vabVCA = 'ITL'
095600011113     C                   EVAL      vabCAS = *zeros
095700011113     C                   ENDIF
095800011113     C                   ENDIF
095900011113     C*
096000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
096100011113     C                   Z-ADD     vabVMD        wrkDec
096200011113     C                   IF        wrkDec > *zeros
096300011113     C                   IF        vabVAD = 'ITL'
096400011113     C                   EVAL      vabVMD = *zeros
096500011113     C                   ENDIF
096600011113     C                   ENDIF
096700011113     C*
096800011113     C                   ENDSR
096900011113     C***
097000011113
097100011113
097200000801
097300000801
097400990920      /TITLE Invio dei dati al punto operativo.
097500010202     C     invio         BEGSR
097600990920     C*
097700180220     C* 1° invio FIVAD
097800180220     C* solo se è richiesta sol. B2 (dettaglio colli non sequeziale)
097900180220     C                   IF        %subst(vlrppt:16:2) = 'B2'
098000010201     C                   reset                   dscmz
098100180220     C                   move      VADLNP        cmzdst
098200180220     C                   eval      cmzfld = 'FIVADWWR'
098300010201     C                   eval      cmzmbd = vlrhdl
098400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
098500021009     C***                if        prmfir = *blanks
098600180220     C                   eval      cmzfla = 'FIVAD00F'
098700180220     C                   eval      cmzmba = 'FIVAD00F'
098800021009     C***                else
098900021009     C***                eval      cmzfla = prmfir
099000021009     C***                eval      cmzmba = prmfir
099100021009     C***                endif
099200010201     C                   eval      cmznrr = *zeros
099300020305     C                   move      §ctrokvt      cmznrr
099400021018     C                   eval      cmzlba = vlrfl1
099500010201     C                   call(e)   'TIS711C'
099600010201     C                   parm                    dscmz
099700010201     C                   parm      *blanks       esito
099800010205     C                   if        %error
099900010205     C                             or cmzerr = '1'
100000010205     C                             or esito  = '1'
100100010205     C                   eval      wrkesito = '3'
100200180220     C                   endif
100300180220     C                   endif
100400180220     C*
100500180220     C* 2° invio FIVAT
100600180220     C* (solo se ho inviato VAD c'è la possibilità che wrkesito sia 3=errore)
100700180220     C                   IF        wrkesito <> '3'
100800180220     C                   reset                   dscmz
100900180220     C                   move      VATLNP        cmzdst
101000180220     C                   eval      cmzfld = 'FIVATWWR'
101100180220     C                   eval      cmzmbd = vlrhdl
101200180220     C                   eval      %subst(cmzmbd:1:1) = 'M'
101300180220     C***                if        prmfir = *blanks
101400180220     C                   eval      cmzfla = 'FIVAT00F'
101500180220     C                   eval      cmzmba = 'FIVAT00F'
101600180220     C***                else
101700180220     C***                eval      cmzfla = prmfir
101800180220     C***                eval      cmzmba = prmfir
101900180220     C***                endif
102000180220     C                   eval      cmznrr = *zeros
102100180220     C                   move      §ctrokvt      cmznrr
102200180220     C                   eval      cmzlba = vlrfl1
102300180220     C                   call(e)   'TIS711C'
102400180220     C                   parm                    dscmz
102500180220     C                   parm      *blanks       esito
102600180220     C                   if        %error
102700180220     C                             or cmzerr = '1'
102800180220     C                             or esito  = '1'
102900180220     C                   eval      wrkesito = '3'
103000180220     C                   else
103100010201     C*
103200180220     C* 3° invio FIVAB
103300010201     C                   reset                   dscmz
103400051228     C                   move      VABLNP        cmzdst
103500010201     C                   eval      cmzfld = vlrfou
103600010201     C                   eval      cmzmbd = vlrhdl
103700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
103800021009     C***                if        prmfir = *blanks
103900021113     C                   eval      cmzfla = 'FIVAB00F'
104000021113     C                   eval      cmzmba = 'FIVAB00F'
104100021009     C***                else
104200021009     C***                eval      cmzfla = prmfir
104300021009     C***                eval      cmzmba = prmfir
104400021009     C***                endif
104500010201     C                   eval      cmznrr = *zeros
104600010201     C                   move      §ctrokvb      cmznrr
104700021018     C                   eval      cmzlba = vlrfl1
104800010201     C                   call(e)   'TIS711C'
104900010201     C                   parm                    dscmz
105000010201     C                   parm      *blanks       esito
105100010201     C                   if        %error
105200010201     C                             or cmzerr = '1'
105300010201     C                             or esito  = '1'
105400010201     C                   eval      wrkesito = '3'
105500010201     C                   endif
105600010205     C                   endif
105700180220     C                   endif
105800990920     C*
105900000613     C                   ENDSR
106000000613     C***
106100070411
106200070411     C     *pssr         BEGSR
106300070411     C*
106400070411     C                   if        %open(tivin00r)
106500070411     C                   close     tivin00r
106600070411     C                   endif
106700070411     C                   if        %open(fivabwwr)
106800070411     C                   close     fivabwwr
106900070411     C                   endif
107000070411     C                   if        %open(fivatwwr)
107100070411     C                   close     fivatwwr
107200070411     C                   endif
107300180219     C                   if        %open(fivadwwr)
107400180219     C                   close     fivadwwr
107500180219     C                   endif
107600070411     C*
107700070411     C* Effettuo la chiamata al CLLE preposto
107800180220     C                   SELECT
107900180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
108000180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
108100180220     C                   call(e)   'TITVVDTC'
108200180220     C                   parm                    parccm
108300180220     C                   parm                    parmbr
108400180220     C                   parm      '2'           paropz
108500180220     C                   OTHER
108600180220     C                   call(e)   'TITVVTC'
108700180220     C                   parm                    parccm
108800180220     C                   parm                    parmbr
108900180220     C                   parm      '2'           paropz
109000180220     C                   ENDSL
109100070411     C*
109200070411     C                   eval      wrkesito = '2'
109300070411     C*
109400070411     C                   seton                                        LR
109500070411     C*
109600070411     C                   ENDSR     '*CANCL'
109700070411     C***
109800070411
109900990910
110000000613     C     *inzsr        BEGSR
110100990910     C*
110200990910     C     *entry        plist
110300990920     C                   parm                    tivlrds
110400990921     C                   parm      wrkesito      esito
110500000724     C                   parm                    prmlit
110600000710     C                   parm                    prmfir
110700000613     C*
110800000830     C* CALCOLA LA DATA CORRENTE
110900121030     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
111000121030     C                   eval      datcor = %dec(%date() : *ISO)
111100000830     C*
111200000613     C                   ENDSR
111300000613     C***
