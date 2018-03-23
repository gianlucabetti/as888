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
001900990921     D wrkesito        s                   like(esito)
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
035800051228     C*
035900051206     C                   MOVEL     '7Q'          VABCTM
036000051206     C                   MOVEL     '1'           VABCBO
036100020619     C* ... e poi verifico se sono stati passati come parametri
036200020619     C                   IF        vlrppt > *blanks
036300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
036400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036500020619     C                   EXSR      CHKNUM
036600020619     C                   IF        PiInt=*on
036700020619     C                   Z-ADD     PiVal         VABCCM
036800020619     C                   Z-ADD     PiVal         VATCCM
036900180220     C                   Z-ADD     PiVal         VADCCM
037000020619     C                   ENDIF
037100040506     C                   ENDIF
037200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
037300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037400020619     C                   EXSR      CHKNUM
037500020619     C                   IF        PiInt=*on
037600020619     C                   Z-ADD     PiVal         VABLNP
037700020619     C                   Z-ADD     PiVal         VATLNP
037800180220     C                   Z-ADD     PiVal         VADLNP
037900040506     C                   ENDIF
038000020619     C                   ENDIF
038100040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
038200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038300020619     C                   EXSR      CHKNUM
038400020619     C                   IF        PiInt=*on
038500020619     C                   Z-ADD     PiVal         VABCTR
038600040506     C                   ENDIF
038700020619     C                   ENDIF
038800041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
038900041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
039000041210     C                   ENDIF
039100020619     C                   ENDIF
039200000801     C*
039300000801     C                   ENDSR
039400000801     C*----------------------------------------------------*
039500040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
039600000801     C*----------------------------------------------------*
039700040910     C     IMPVABVAT     BEGSR
039800041210     C*
039900041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
040000041210     C                   Z-ADD     *zeros        errore            1 0
040100051213     C*
040200051213     C********
040300051213     C* Tipo record 'VIA' (riconoscimento cliente mittente)
040400051213     C********
040500051213     C                   IF        %trim(%subst(vindta:1:3)) = 'VIA'
040600051228     C                   SETOFF                                       202122
040700051228     C                   SETOFF                                       232425
040800051228     C                   SETOFF                                       262728
040900051228     C                   SETOFF                                       293031
041000051228     C                   SETOFF                                       323334
041100051228     C                   SETOFF                                       353637
041200101012     C                   SETOFF                                       3839
041300120213     C                   SETOFF                                       404142
041400120213     C                   SETOFF                                       434445
041500120213     C                   SETOFF                                       464748
041600120720     C                   SETOFF                                       495051
041700121030     C                   SETOFF                                       52
041800080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
041900051228     C                             %trim(%subst(vindta:116:5)) = 'BOSCH'
042000051228     C                   SETON                                        20
042100051213     C                   ENDIF
042200080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
042300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042400051228     C                   SETON                                        21
042500051228     C                   ENDIF
042600080109     C                   IF        %trim(%subst(vindta:54:2)) = '17' AND
042700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042800051228     C                   SETON                                        22
042900051228     C                   ENDIF
043000080109     C                   IF        %trim(%subst(vindta:54:2)) = '11' AND
043100051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043200051228     C                   SETON                                        23
043300051228     C                   ENDIF
043400080109     C                   IF        %trim(%subst(vindta:54:2)) = '10' AND
043500051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043600051228     C                   SETON                                        24
043700051228     C                   ENDIF
043800080109     C                   IF        %trim(%subst(vindta:54:2)) = '08' AND
043900051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044000051228     C                   SETON                                        25
044100051228     C                   ENDIF
044200080109     C                   IF        %trim(%subst(vindta:54:2)) = '04' AND
044300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044400051228     C                   SETON                                        26
044500051228     C                   ENDIF
044600080109     C                   IF        %trim(%subst(vindta:54:2)) = '02' AND
044700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044800051228     C                   SETON                                        27
044900051228     C                   ENDIF
045000080109     C                   IF        %trim(%subst(vindta:54:2)) = '03'      AND
045100051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045200051228     C                   SETON                                        29
045300051228     C                   ENDIF
045400080109     C                   IF        %trim(%subst(vindta:54:2)) = '05'      AND
045500051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045600051228     C                   SETON                                        31
045700051228     C                   ENDIF
045800100223     C                   IF        %trim(%subst(vindta:54:2)) = '34'      AND
045900100223     C                             %trim(%subst(vindta:116:5)) = *blanks
046000100223     C                   SETON                                        34
046100100223     C                   ENDIF
046200080109     C                   IF        %trim(%subst(vindta:54:2)) = '12'      AND
046300051228     C                             %trim(%subst(vindta:116:5)) = *blanks
046400051228     C                   SETON                                        35
046500051228     C                   ENDIF
046600080109     C                   IF        %trim(%subst(vindta:54:2)) = '13'      AND
046700051228     C                             %trim(%subst(vindta:116:5)) = *blanks
046800051228     C                   SETON                                        36
046900051228     C                   ENDIF
047000080109     C                   IF        %trim(%subst(vindta:54:2)) = '18'      AND
047100051228     C                             %trim(%subst(vindta:116:5)) = *blanks
047200051228     C                   SETON                                        38
047300051228     C                   ENDIF
047400110607     C***                IF        %trim(%subst(vindta:54:2)) = '06'
047500110607     C                   IF        %trim(%subst(vindta:54:2)) = '75'
047600101012     C                   SETON                                        39
047700101012     C                   ENDIF
047800120213     C                   IF        %trim(%subst(vindta:54:2)) = '10'
047900120213     C                   SETON                                        40
048000120213     C                   ENDIF
048100120213     C                   IF        %trim(%subst(vindta:54:2)) = '36'
048200120213     C                   SETON                                        41
048300120213     C                   ENDIF
048400120213     C                   IF        %trim(%subst(vindta:54:2)) = '05'
048500120213     C                   SETON                                        42
048600120213     C                   ENDIF
048700120213     C                   IF        %trim(%subst(vindta:54:2)) = '72'
048800120213     C                   SETON                                        43
048900120213     C                   ENDIF
049000120213     C                   IF        %trim(%subst(vindta:54:2)) = '38'
049100120213     C                   SETON                                        44
049200120213     C                   ENDIF
049300120213     C                   IF        %trim(%subst(vindta:54:2)) = '20'
049400120213     C                   SETON                                        45
049500120213     C                   ENDIF
049600120213     C                   IF        %trim(%subst(vindta:54:2)) = '23'
049700120213     C                   SETON                                        46
049800120213     C                   ENDIF
049900120213     C                   IF        %trim(%subst(vindta:54:2)) = '09'
050000120213     C                   SETON                                        47
050100120213     C                   ENDIF
050200120213     C                   IF        %trim(%subst(vindta:54:2)) = '08'
050300120213     C                   SETON                                        48
050400120213     C                   ENDIF
050500120213     C                   IF        %trim(%subst(vindta:54:2)) = '02'
050600120213     C                   SETON                                        49
050700120213     C                   ENDIF
050800120213     C                   IF        %trim(%subst(vindta:54:2)) = '15'
050900120213     C                   SETON                                        50
051000120213     C                   ENDIF
051100120720     C                   IF        %trim(%subst(vindta:54:2)) = '36'
051200120720     C                   SETON                                        51
051300120720     C                   ENDIF
051400121030     C                   IF        %trim(%subst(vindta:54:2)) = '37'
051500121030     C                   SETON                                        52
051600121030     C                   ENDIF
051700101012     C                   ENDIF
051800051213     C********
051900051213     C* FINE Tipo record 'VIA'
052000051213     C********
052100040910     C*
052200041210     C********
052300051206     C* Tipo record 'PR1' (riferimenti, dati destinatario, importi e particolarità consegna)
052400041210     C********
052500051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR1'
052600041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
052700041210     C                   IF        FlgNewBol = '1'
052800041210     C                   WRITE     FIVAB000
052900041210     C                   EVAL      FlgNewBol = '0'
053000041210     C                   ENDIF
053100041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
053200040910     C                   EXSR      INZVAR
053300040910     C                   EXSR      DEFCAM
053400041210     C                   EVAL      FlgNewBol = '1'
053500040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
053600040928     C                   MOVEL     datcor        VABAAS
053700040928     C                   MOVEL     datcor        VATAAS
053800040928     C                   MOVE      datcor        VABMGS
053900051228     C                   MOVE(P)   VABLNP        VABFGS
054000051228     C                   MOVE(P)   VABLNP        VATFGS
054100180219     C                   MOVE(P)   VABLNP        VADFGS
054200180219     C                   MOVEL     datcor        VADAAS
054300180227     C*
054400180227     C* Dati CHIAVE esclusivi SOLO per alcuni clienti
054500180227     C                   SELECT
054600180227     C                   WHEN      VABCCM = 2230606
054700180227     C* ......VABNRS
054800180227     C                   EVAL      PiStr=%trim(%subst(vindta:873:2))
054900180227     C                   EXSR      CHKNUM
055000180227     C                   IF        PiInt=*on
055100180227     C                   Z-ADD     PiVal         VABNRS
055200180227     C                   Z-ADD     PiVal         VATNRS
055300180227     C                   Z-ADD     PiVal         VADNRS
055400180227     C                   ELSE
055500180227     C                   ADD       1             errore
055600180227     C                   EVAL      vinmsg = %trimr(vinmsg)
055700180227     C                             + ' ' + 'VABNRS VATNRS VADNRS'
055800180227     C                   ENDIF
055900180227     C*
056000180227     C                   ENDSL
056100180227     C*
056200051206     C* ......VABNSP/VATNSP/VABRMN/VABRMA
056300051206     C                   EVAL      VABRMA=%trim(%subst(vindta:4:27))
056400051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
056500051206     C                   EXSR      CHKNUM
056600051206     C                   IF        PiInt=*on
056700051206     C                   Z-ADD     PiVal         VABRMN
056800051206     C                   ELSE
056900051206     C                   ADD       1             errore
057000051206     C                   EVAL      vinmsg = %trimr(vinmsg)
057100051206     C                             + ' ' + 'VABRMN'
057200051206     C                   ENDIF
057300051206     C*
057400051206     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
057500051206     C                   EXSR      CHKNUM
057600051206     C                   IF        PiInt=*on
057700051206     C                   Z-ADD     PiVal         VABNSP
057800051206     C                   Z-ADD     PiVal         VATNSP
057900180219     C                   Z-ADD     PiVal         VADNSP
058000041210     C                   ELSE
058100041210     C                   ADD       1             errore
058200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
058300180219     C                             + ' ' + 'VABNSP VATNSP'
058400041210     C                   ENDIF
058500051206     C* ......VABRMO
058600051206     C                   IF        %trim(%subst(vindta:46:35)) <>
058700051206     C                             'GEODIS ZUST AMBROSETTI'
058800051206     C                   EVAL      VABRMO=%trim(%subst(vindta:46:35))
058900051206     C                   ENDIF
059000051206     C* ......VATNOT + VATTRC = 'B'
059100051206     C                   IF        %trim(%subst(vindta:272:35)) <> *blanks
059200051206     C                   EVAL      VATNOT=%trim(%subst(vindta:272:35))
059300051206     C                   EVAL      VATTRC = 'B'
059400051206     C                   WRITE     FIVAT000
059500051206     C                   ENDIF
059600051206     C* ......VABRSD
059700051206     C                   EVAL      VABRSD=%trim(%subst(vindta:428:35))
059800050805     C* ......VABIND
059900051206     C                   EVAL      VABIND=%trim(%subst(vindta:498:35))
060000050805     C* ......VABLOD
060100051206     C                   EVAL      VABLOD=%trim(%subst(vindta:578:35))
060200050805     C* ......VABCAD
060300051206     C                   EVAL      PiStr=%trim(%subst(vindta:568:10))
060400050805     C                   EXSR      CHKNUM
060500050805     C                   IF        PiInt=*on
060600050805     C                   Z-ADD     PiVal         Num5_0
060700050805     C                   MOVEL(P)  Num5_0        VABCAD
060800050805     C                   ELSE
060900050805     C                   ADD       1             errore
061000050805     C                   EVAL      vinmsg = %trimr(vinmsg)
061100050805     C                             + ' ' + 'VABCAD'
061200050805     C                   ENDIF
061300051206     C* ......VABPRD
061400051206     C                   EVAL      VABPRD=%trim(%subst(vindta:613:3))
061500051206     C* ......VABCBO
061600051206     C                   IF        %trim(%subst(vindta:619:3)) = 'DDU'
061700051206     C                   EVAL      VABCBO='1'
061800051206     C                   ENDIF
061900051206     C                   IF        %trim(%subst(vindta:619:3)) = 'EXW'
062000051206     C                   EVAL      VABCBO='2'
062100051206     C                   ENDIF
062200051206     C* ......VABCAS/VABVCA
062300051206     C                   IF        %trim(%subst(vindta:628:15)) <> *zeros
062400051206     C                   EVAL      FlgCAS = '1'
062500051206     C*
062600051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBK'
062700051206     C                   EVAL      VABTIC='OM'
062800051206     C                   ENDIF
062900051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBC'
063000051206     C                   EVAL      VABTIC='OM'
063100051206     C                   ENDIF
063200051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YCB'
063300051206     C                   EVAL      VABTIC='OC'
063400051206     C                   ENDIF
063500051206     C                   IF        %trim(%subst(vindta:643:3)) = 'CSH'
063600051206     C                   EVAL      VABTIC=*blanks
063700051206     C                   ENDIF
063800051206     C*
063900051206     C                   EVAL      VABVCA=%trim(%subst(vindta:625:3))
064000051206     C*
064100051206     C                   EVAL      PiStr=%trim(%subst(vindta:628:15))
064200051206     C                   EXSR      CHKNUM
064300051206     C                   IF        PiNum=*on
064400051206     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
064500051206     C                   Z-ADD     PiVal         VABCAS
064600051206     C                   ELSE
064700051206     C                   ADD       1             errore
064800051206     C                   EVAL      vinmsg = %trimr(vinmsg)
064900051206     C                             + ' ' + 'VABCAS'
065000051206     C                   ENDIF
065100051206     C                   ENDIF
065200180227     C*
065300180227     C* Dati NON CHIAVE esclusivi SOLO per alcuni clienti
065400180227     C                   SELECT
065500180227     C                   WHEN      VABCCM = 2230606
065600180227     C* ......VABLNA
065700180227     C                   EVAL      PiStr=%trim(%subst(vindta:875:3))
065800180227     C                   EXSR      CHKNUM
065900180227     C                   IF        PiInt=*on
066000180227     C                   Z-ADD     PiVal         VABLNA
066100180227     C                   ELSE
066200180227     C                   ADD       1             errore
066300180227     C                   EVAL      vinmsg = %trimr(vinmsg)
066400180227     C                             + ' ' + 'VABLNA'
066500180227     C                   ENDIF
066600180227     C* ......VABZNC
066700180227     C                   EVAL      PiStr=%trim(%subst(vindta:878:2))
066800180227     C                   EXSR      CHKNUM
066900180227     C                   IF        PiInt=*on
067000180227     C                   Z-ADD     PiVal         VABZNC
067100180227     C                   ELSE
067200180227     C                   ADD       1             errore
067300180227     C                   EVAL      vinmsg = %trimr(vinmsg)
067400180227     C                             + ' ' + 'VABZNC'
067500180227     C                   ENDIF
067600180227     C* ......VATNOT + VATTRC = 'P'
067700180227     C                   IF        %trim(%subst(vindta:856:17)) <> *blanks
067800180227     C                   EVAL      VATNOT=%trim(%subst(vindta:856:17))
067900180227     C                   EVAL      %subst(VATNOT:34:2)='10'
068000180227     C                   EVAL      VATTRC = 'P'
068100180227     C                   WRITE     FIVAT000
068200180227     C                   ENDIF
068300180227     C*
068400180227     C                   ENDSL
068500180219     C*
068600060307     C* Considerazioni sul contenuto di campi precedentemente valorizzati
068700060307     C                   IF        FlgCAS <> '0'
068800060307     C                   IF        VABCBO = '1'
068900060307     C                   EVAL      VABCBO = '4'
069000060307     C                   ELSE
069100060307     C                   EVAL      VABCBO = '6'
069200060307     C                   ENDIF
069300060307     C                   ENDIF
069400051206     C*
069500051206     C                   ENDIF
069600051206     C********
069700051206     C* FINE Tipo record 'PR1'
069800051206     C********
069900051206     C*
070000051206     C********
070100051206     C* Tipo record 'PR2' (note, consegna richiesta, valore da assicurare)
070200051206     C********
070300051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR2'
070400051206     C* ......VABDCR
070500051213     C                   IF        %subst(vindta:7:8)<>*blanks
070600051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
070700050805     C                   EXSR      CHKNUM
070800051206     C                   IF        PiInt=*on
070900051206     C                   Z-ADD     PiVal         VABDCR
071000050805     C                   ELSE
071100050805     C                   ADD       1             errore
071200050805     C                   EVAL      vinmsg = %trimr(vinmsg)
071300051206     C                             + ' ' + 'VABDCR'
071400050805     C                   ENDIF
071500051213     C                   ENDIF
071600051206     C* ......VABHCR
071700051213     C                   IF        %subst(vindta:7+8:4)<>*blanks
071800051206     C                   EVAL      PiStr=%trim(%subst(vindta:7+8:4))
071900051206     C                   EXSR      CHKNUM
072000051206     C                   IF        PiInt=*on
072100051206     C                   Z-ADD     PiVal         VABHCR
072200051206     C                   ELSE
072300051206     C                   ADD       1             errore
072400051206     C                   EVAL      vinmsg = %trimr(vinmsg)
072500051206     C                             + ' ' + 'VABHCR'
072600051206     C                   ENDIF
072700051213     C                   ENDIF
072800051206     C* ......VABNOT
072900051206     C                   EVAL      VABNOT=%trim(%subst(vindta:22:35))
073000051206     C* ......VABNT2
073100051206     C                   EVAL      VABNT2=%trim(%subst(vindta:22+35:35))
073200051206     C* ......VABIAS/VABVAS
073300051206     C                   IF        %subst(vindta:845:1) = 'S'
073400051206     C                   EVAL      VABVAS=%trim(%subst(vindta:846:3))
073500051206     C                   EVAL      PiStr=%trim(%subst(vindta:849:15))
073600051206     C                   EXSR      CHKNUM
073700051206     C                   IF        PiNum=*on
073800051206     C                   EVAL      PiVal = PiVal / 100
073900051206     C                   Z-ADD     PiVal         VABIAS
074000051206     C                   ELSE
074100051206     C                   ADD       1             errore
074200051206     C                   EVAL      vinmsg = %trimr(vinmsg)
074300051206     C                             + ' ' + 'VABIAS'
074400051206     C                   ENDIF
074500051206     C                   ENDIF
074600051206     C*
074700051206     C                   ENDIF
074800051206     C********
074900051206     C* FINE Tipo record 'PR2'
075000051206     C********
075100051206     C*
075200051206     C********
075300051206     C* Tipo record 'MER' (colli, peso)
075400051206     C********
075500051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MER'
075600050805     C* ......VABNCL
075700051206     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
075800050805     C                   EXSR      CHKNUM
075900050805     C                   IF        PiInt=*on
076000080208     C***                Z-ADD     PiVal         VABNCL
076100080208     C                   ADD       PiVal         VABNCL
076200050805     C                   ELSE
076300050805     C                   ADD       1             errore
076400050805     C                   EVAL      vinmsg = %trimr(vinmsg)
076500050805     C                             + ' ' + 'VABNCL'
076600050805     C                   ENDIF
076700050805     C* ......VABPKB
076800051206     C                   EVAL      PiStr=%trim(%subst(vindta:15:10))
076900050805     C                   EXSR      CHKNUM
077000050805     C                   IF        PiNum=*on
077100051206     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
077200080208     C***                Z-ADD     PiVal         VABPKB
077300080208     C                   ADD       PiVal         VABPKB
077400050805     C                   ELSE
077500050805     C                   ADD       1             errore
077600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
077700050805     C                             + ' ' + 'VABPKB'
077800050805     C                   ENDIF
077900061012     C* ......VABVLB
078000061012     C                   EVAL      PiStr=%trim(%subst(vindta:35:10))
078100061012     C                   EXSR      CHKNUM
078200061012     C                   IF        PiNum=*on
078300061012     C                   EVAL      PiVal = PiVal / 1000                         * da decim3 a metri3
078400080208     C***                Z-ADD     PiVal         VABVLB
078500080208     C                   ADD       PiVal         VABVLB
078600061012     C                   ELSE
078700061012     C                   ADD       1             errore
078800061012     C                   EVAL      vinmsg = %trimr(vinmsg)
078900061012     C                             + ' ' + 'VABVLB'
079000061012     C                   ENDIF
079100050805     C*
079200050805     C                   ENDIF
079300051206     C********
079400051206     C* FINE Tipo record 'MER'
079500051206     C********
079600051206     C*
079700051206     C********
079800051206     C* Tipo record 'MAR' (barcode)
079900051206     C********
080000051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MAR'
080100180219     C* e non è il tipo rcd MAR24 che non ha il barcode ma il collo
080200180219     C                             and %trim(%subst(vindta:1:5)) <>'MAR24'
080300051206     C* ......VATNOT + VATTRC = 'E'
080400051213     C                   IF        %trim(%subst(vindta:7:20)) <> *blanks
080500051213     C                   EVAL      VATNOT=%trim(%subst(vindta:7:20))
080600051206     C                   EVAL      VATTRC = 'E'
080700051206     C                   WRITE     FIVAT000
080800051206     C                   ENDIF
080900050805     C*
081000050805     C                   ENDIF
081100051206     C********
081200051206     C* FINE Tipo record 'MAR'
081300051206     C********
081400180219     C*
081500180219     C********
081600180219     C* Tipo record 'MAR24' (marcatura alternativa)
081700180219     C********
081800180219     C                   IF        %trim(%subst(vindta:1:5)) = 'MAR24'
081900180219     C* devo essere in sol.B2
082000180219     C                             and %subst(vlrppt:16:2) = 'B2'
082100180219     C* ......VADNCD
082200180219     C                   IF        %trim(%subst(vindta:50:7)) <> *blanks
082300180219     C                   EVAL      PiStr=%trim(%subst(vindta:50:7))
082400180219     C                   EXSR      CHKNUM
082500180219     C                   IF        PiInt=*on
082600180219     C                   Z-ADD     PiVal         VADNCD
082700180227     C                   Z-ADD     1             VADNCL
082800180220     C                   WRITE     FIVAD000
082900180219     C                   ELSE
083000180219     C                   ADD       1             errore
083100180219     C                   EVAL      vinmsg = %trimr(vinmsg)
083200180219     C                             + ' ' + 'VADNCD'
083300180219     C                   ENDIF
083400180219     C                   ENDIF
083500180219     C*
083600180219     C                   ENDIF
083700180219     C********
083800180219     C* FINE Tipo record 'MAR'
083900180219     C********
084000040910     C*
084100040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
084200040910     C                   EXSR      CHKIMPDIV
084300010202     C*
084400000801     C* Ebbene...
084500000801     C                   ADD       1             §CTRMO
084600010201     C                   IF        errore <> *zeros
084700000801     C                   ADD       1             §CTRNO
084800000801     C                   EVAL      vinflg = '2'
084900000801     C                   ELSE
085000010201     C                   ADD       1             §CTROKVB
085100000801     C                   ENDIF
085200000801     C*
085300000801     C                   ENDSR
085400010202     C*----------------------------------------------------*
085500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
085600010202     C*----------------------------------------------------*
085700020305     C     PREVAT        BEGSR
085800010202     C*
085900021113     C* Compongo il nome del membro da dare al FIVATWWR
086000010202     C                   eval      parmbr = vlrhdl
086100010202     C                   movel     'M'           parmbr
086200041210     C                   eval      parccm = vlrksc
086300010202     C                   eval      paropz = '1'
086400010202     C* Effettuo la chiamata al CLLE preposto
086500180220     C                   SELECT
086600180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
086700180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
086800180220     C                   call(e)   'TITVVDTC'
086900180220     C                   parm                    parccm
087000180220     C                   parm                    parmbr
087100180220     C                   parm                    paropz
087200180220     C                   OTHER
087300180220     C                   call(e)   'TITVVTC'
087400180220     C                   parm                    parccm
087500180220     C                   parm                    parmbr
087600180220     C                   parm                    paropz
087700180220     C                   ENDSL
087800180220     C*
087900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
088000010202     C                   if        %error
088100010202     C                   movel     '1'           chkcall
088200010202     C                   else
088300010202     C                   movel     '0'           chkcall
088400010202     C                   endif
088500010202     C*
088600010202     C                   ENDSR
088700000801     C*----------------------------------------------------*
088800000801     C*  CONTROLLO NUMERICITA' CAMPI
088900000801     C*----------------------------------------------------*
089000000801     C     CHKNUM        BEGSR
089100000801     C*
089200000801     C                   call(e)   'ISNUMERIC'
089300000801     C                   PARM                    PiStr            30
089400041210     C                   PARM      '.'           PiDecChr          1
089500000801     C                   PARM      *ZEROS        PiVal            30 9
089600000801     C                   PARM      '0'           PiInt             1
089700000801     C                   PARM      '0'           PiNum             1
089800000801     C                   IF        %error
089900000801     C                   EVAL      PiInt=*off
090000000801     C                   ENDIF
090100000801     C*
090200000801     C                   ENDSR
090300000801     C***
090400000801
090500011113
090600011113     C*----------------------------------------------------*
090700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
090800011113     C*----------------------------------------------------*
090900011113     C     CHKIMPDIV     BEGSR
091000011113     C*
091100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
091200011113     C                   Z-ADD     *zeros        wrkDec            9 9
091300011113     C*
091400011113     C* Come prima cosa effettuo considerazioni sulla divisa
091500011113     C                   IF        vabIAS > *zeros
091600011113     C                   IF        vabVAS <> 'EUR'
091700011113     C                   EVAL      vabVAS =  'ITL'
091800011113     C                   ENDIF
091900011113     C                   ENDIF
092000011113     C*
092100011113     C                   IF        vabCAS > *zeros
092200011113     C                   IF        vabVCA <> 'EUR'
092300011113     C                   EVAL      vabVCA =  'ITL'
092400011113     C                   ENDIF
092500011113     C                   ENDIF
092600011113     C*
092700011113     C                   IF        vabVMD > *zeros
092800020305     C                   IF        vabVAD <> 'EUR'
092900011113     C                   EVAL      vabVAD =  'ITL'
093000011113     C                   ENDIF
093100011113     C                   ENDIF
093200011113     C*
093300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
093400011113     C                   Z-ADD     vabIAS        wrkDec
093500011113     C                   IF        wrkDec > *zeros
093600011113     C                   IF        vabVAS = 'ITL'
093700011113     C                   EVAL      vabIAS = *zeros
093800011113     C                   ENDIF
093900011113     C                   ENDIF
094000011113     C*
094100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
094200011113     C                   Z-ADD     vabCAS        wrkDec
094300011113     C                   IF        wrkDec > *zeros
094400011113     C                   IF        vabVCA = 'ITL'
094500011113     C                   EVAL      vabCAS = *zeros
094600011113     C                   ENDIF
094700011113     C                   ENDIF
094800011113     C*
094900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
095000011113     C                   Z-ADD     vabVMD        wrkDec
095100011113     C                   IF        wrkDec > *zeros
095200011113     C                   IF        vabVAD = 'ITL'
095300011113     C                   EVAL      vabVMD = *zeros
095400011113     C                   ENDIF
095500011113     C                   ENDIF
095600011113     C*
095700011113     C                   ENDSR
095800011113     C***
095900011113
096000011113
096100000801
096200000801
096300990920      /TITLE Invio dei dati al punto operativo.
096400010202     C     invio         BEGSR
096500990920     C*
096600180220     C* 1° invio FIVAD
096700180220     C* solo se è richiesta sol. B2 (dettaglio colli non sequeziale)
096800180220     C                   IF        %subst(vlrppt:16:2) = 'B2'
096900010201     C                   reset                   dscmz
097000180220     C                   move      VADLNP        cmzdst
097100180220     C                   eval      cmzfld = 'FIVADWWR'
097200010201     C                   eval      cmzmbd = vlrhdl
097300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
097400021009     C***                if        prmfir = *blanks
097500180220     C                   eval      cmzfla = 'FIVAD00F'
097600180220     C                   eval      cmzmba = 'FIVAD00F'
097700021009     C***                else
097800021009     C***                eval      cmzfla = prmfir
097900021009     C***                eval      cmzmba = prmfir
098000021009     C***                endif
098100010201     C                   eval      cmznrr = *zeros
098200020305     C                   move      §ctrokvt      cmznrr
098300021018     C                   eval      cmzlba = vlrfl1
098400010201     C                   call(e)   'TIS711C'
098500010201     C                   parm                    dscmz
098600010201     C                   parm      *blanks       esito
098700010205     C                   if        %error
098800010205     C                             or cmzerr = '1'
098900010205     C                             or esito  = '1'
099000010205     C                   eval      wrkesito = '3'
099100180220     C                   endif
099200180220     C                   endif
099300180220     C*
099400180220     C* 2° invio FIVAT
099500180220     C* (solo se ho inviato VAD c'è la possibilità che wrkesito sia 3=errore)
099600180220     C                   IF        wrkesito <> '3'
099700180220     C                   reset                   dscmz
099800180220     C                   move      VATLNP        cmzdst
099900180220     C                   eval      cmzfld = 'FIVATWWR'
100000180220     C                   eval      cmzmbd = vlrhdl
100100180220     C                   eval      %subst(cmzmbd:1:1) = 'M'
100200180220     C***                if        prmfir = *blanks
100300180220     C                   eval      cmzfla = 'FIVAT00F'
100400180220     C                   eval      cmzmba = 'FIVAT00F'
100500180220     C***                else
100600180220     C***                eval      cmzfla = prmfir
100700180220     C***                eval      cmzmba = prmfir
100800180220     C***                endif
100900180220     C                   eval      cmznrr = *zeros
101000180220     C                   move      §ctrokvt      cmznrr
101100180220     C                   eval      cmzlba = vlrfl1
101200180220     C                   call(e)   'TIS711C'
101300180220     C                   parm                    dscmz
101400180220     C                   parm      *blanks       esito
101500180220     C                   if        %error
101600180220     C                             or cmzerr = '1'
101700180220     C                             or esito  = '1'
101800180220     C                   eval      wrkesito = '3'
101900180220     C                   else
102000010201     C*
102100180220     C* 3° invio FIVAB
102200010201     C                   reset                   dscmz
102300051228     C                   move      VABLNP        cmzdst
102400010201     C                   eval      cmzfld = vlrfou
102500010201     C                   eval      cmzmbd = vlrhdl
102600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
102700021009     C***                if        prmfir = *blanks
102800021113     C                   eval      cmzfla = 'FIVAB00F'
102900021113     C                   eval      cmzmba = 'FIVAB00F'
103000021009     C***                else
103100021009     C***                eval      cmzfla = prmfir
103200021009     C***                eval      cmzmba = prmfir
103300021009     C***                endif
103400010201     C                   eval      cmznrr = *zeros
103500010201     C                   move      §ctrokvb      cmznrr
103600021018     C                   eval      cmzlba = vlrfl1
103700010201     C                   call(e)   'TIS711C'
103800010201     C                   parm                    dscmz
103900010201     C                   parm      *blanks       esito
104000010201     C                   if        %error
104100010201     C                             or cmzerr = '1'
104200010201     C                             or esito  = '1'
104300010201     C                   eval      wrkesito = '3'
104400010201     C                   endif
104500010205     C                   endif
104600180220     C                   endif
104700990920     C*
104800000613     C                   ENDSR
104900000613     C***
105000070411
105100070411     C     *pssr         BEGSR
105200070411     C*
105300070411     C                   if        %open(tivin00r)
105400070411     C                   close     tivin00r
105500070411     C                   endif
105600070411     C                   if        %open(fivabwwr)
105700070411     C                   close     fivabwwr
105800070411     C                   endif
105900070411     C                   if        %open(fivatwwr)
106000070411     C                   close     fivatwwr
106100070411     C                   endif
106200180219     C                   if        %open(fivadwwr)
106300180219     C                   close     fivadwwr
106400180219     C                   endif
106500070411     C*
106600070411     C* Effettuo la chiamata al CLLE preposto
106700180220     C                   SELECT
106800180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
106900180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
107000180220     C                   call(e)   'TITVVDTC'
107100180220     C                   parm                    parccm
107200180220     C                   parm                    parmbr
107300180220     C                   parm      '2'           paropz
107400180220     C                   OTHER
107500180220     C                   call(e)   'TITVVTC'
107600180220     C                   parm                    parccm
107700180220     C                   parm                    parmbr
107800180220     C                   parm      '2'           paropz
107900180220     C                   ENDSL
108000070411     C*
108100070411     C                   eval      wrkesito = '2'
108200070411     C*
108300070411     C                   seton                                        LR
108400070411     C*
108500070411     C                   ENDSR     '*CANCL'
108600070411     C***
108700070411
108800990910
108900000613     C     *inzsr        BEGSR
109000990910     C*
109100990910     C     *entry        plist
109200990920     C                   parm                    tivlrds
109300990921     C                   parm      wrkesito      esito
109400000724     C                   parm                    prmlit
109500000710     C                   parm                    prmfir
109600000613     C*
109700000830     C* CALCOLA LA DATA CORRENTE
109800121030     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
109900121030     C                   eval      datcor = %dec(%date() : *ISO)
110000000830     C*
110100000613     C                   ENDSR
110200000613     C***
