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
017100051228     C* Imposto i valori di default...
017200051228     C                   Z-ADD     0053039       VABCCM
017300051228     C                   Z-ADD     0053039       VATCCM
017400051228     C                   Z-ADD     005           VABLNP
017500051228     C                   Z-ADD     005           VATLNP
017600090317     C                   Z-ADD     000           VABCTR
017700180220     C* Traffici IMPORT
017800051228     C   20              Z-ADD     0053578       VABCCM
017900051228     C   20              Z-ADD     0053578       VATCCM
018000051228     C   20              Z-ADD     005           VABLNP
018100051228     C   20              Z-ADD     005           VATLNP
018200051228     C   20              Z-ADD     000           VABCTR
018300051228     C   21              Z-ADD     0053039       VABCCM
018400051228     C   21              Z-ADD     0053039       VATCCM
018500051228     C   21              Z-ADD     005           VABLNP
018600051228     C   21              Z-ADD     005           VATLNP
018700090317     C   21              Z-ADD     000           VABCTR
018800060825     C*  22              Z-ADD     0493248       VABCCM
018900060825     C*  22              Z-ADD     0493248       VATCCM
019000060825     C   22              Z-ADD     1190493       VABCCM
019100060825     C   22              Z-ADD     1190493       VATCCM
019200060307     C   22              Z-ADD     119           VABLNP
019300060307     C   22              Z-ADD     119           VATLNP
019400051228     C   22              Z-ADD     001           VABCTR
019500100223 xxx C***23              Z-ADD     0650826       VABCCM
019600100223 xxx C***23              Z-ADD     0650826       VATCCM
019700100223     C   23              Z-ADD     0650945       VABCCM
019800100223     C   23              Z-ADD     0650945       VATCCM
019900051228     C   23              Z-ADD     065           VABLNP
020000051228     C   23              Z-ADD     065           VATLNP
020100051228     C   23              Z-ADD     001           VABCTR
020200051228     C   24              Z-ADD     0021744       VABCCM
020300051228     C   24              Z-ADD     0021744       VATCCM
020400051228     C   24              Z-ADD     002           VABLNP
020500051228     C   24              Z-ADD     002           VATLNP
020600051228     C   24              Z-ADD     001           VABCTR
020700120110     C   25              Z-ADD     2242028       VABCCM
020800120110     C   25              Z-ADD     2242028       VATCCM
020900120110     C   25              Z-ADD     224           VABLNP
021000120110     C   25              Z-ADD     224           VATLNP
021100051228     C   25              Z-ADD     001           VABCTR
021200051228     C   26              Z-ADD     0012821       VABCCM
021300051228     C   26              Z-ADD     0012821       VATCCM
021400051228     C   26              Z-ADD     001           VABLNP
021500051228     C   26              Z-ADD     001           VATLNP
021600051228     C   26              Z-ADD     001           VABCTR
021700051228     C   27              Z-ADD     1280515       VABCCM
021800051228     C   27              Z-ADD     1280515       VATCCM
021900051228     C   27              Z-ADD     128           VABLNP
022000051228     C   27              Z-ADD     128           VATLNP
022100051228     C   27              Z-ADD     001           VABCTR
022200051228     C* Traffici NAZIONALI
022300051228     C   28              Z-ADD     1280515       VABCCM
022400051228     C   28              Z-ADD     1280515       VATCCM
022500051228     C   28              Z-ADD     128           VABLNP
022600051228     C   28              Z-ADD     128           VATLNP
022700051228     C   28              Z-ADD     001           VABCTR
022800051228     C   29              Z-ADD     0502928       VABCCM
022900051228     C   29              Z-ADD     0502928       VATCCM
023000051228     C   29              Z-ADD     050           VABLNP
023100051228     C   29              Z-ADD     050           VATLNP
023200051228     C   29              Z-ADD     001           VABCTR
023300051228     C   30              Z-ADD     0012821       VABCCM
023400051228     C   30              Z-ADD     0012821       VATCCM
023500051228     C   30              Z-ADD     001           VABLNP
023600051228     C   30              Z-ADD     001           VATLNP
023700051228     C   30              Z-ADD     001           VABCTR
023800051228     C   31              Z-ADD     0939669       VABCCM
023900051228     C   31              Z-ADD     0939669       VATCCM
024000051228     C   31              Z-ADD     093           VABLNP
024100051228     C   31              Z-ADD     093           VATLNP
024200051228     C   31              Z-ADD     001           VABCTR
024300051228     C   32              Z-ADD     0840647       VABCCM
024400051228     C   32              Z-ADD     0840647       VATCCM
024500051228     C   32              Z-ADD     084           VABLNP
024600051228     C   32              Z-ADD     084           VATLNP
024700051228     C   32              Z-ADD     001           VABCTR
024800051228     C   33              Z-ADD     0021744       VABCCM
024900051228     C   33              Z-ADD     0021744       VATCCM
025000051228     C   33              Z-ADD     002           VABLNP
025100051228     C   33              Z-ADD     002           VATLNP
025200051228     C   33              Z-ADD     001           VABCTR
025300100223     C***34              Z-ADD     0650826       VABCCM
025400100223     C***34              Z-ADD     0650826       VATCCM
025500120608     C***34              Z-ADD     0202104       VABCCM
025600120608     C***34              Z-ADD     0202104       VATCCM
025700120608     C   34              Z-ADD     0203067       VABCCM
025800120608     C   34              Z-ADD     0203067       VATCCM
025900100223     C   34              Z-ADD     020           VABLNP
026000100223     C   34              Z-ADD     020           VATLNP
026100051228     C   34              Z-ADD     001           VABCTR
026200051228     C   35              Z-ADD     0063055       VABCCM
026300051228     C   35              Z-ADD     0063055       VATCCM
026400051228     C   35              Z-ADD     006           VABLNP
026500051228     C   35              Z-ADD     006           VATLNP
026600051228     C   35              Z-ADD     001           VABCTR
026700051228     C   36              Z-ADD     0201370       VABCCM
026800051228     C   36              Z-ADD     0201370       VATCCM
026900051228     C   36              Z-ADD     020           VABLNP
027000051228     C   36              Z-ADD     020           VATLNP
027100051228     C   36              Z-ADD     001           VABCTR
027200060825     C*  37              Z-ADD     0493248       VABCCM
027300060825     C*  37              Z-ADD     0493248       VATCCM
027400060825     C   37              Z-ADD     1190493       VABCCM
027500060825     C   37              Z-ADD     1190493       VATCCM
027600060307     C   37              Z-ADD     119           VABLNP
027700060307     C   37              Z-ADD     119           VATLNP
027800051228     C   37              Z-ADD     001           VABCTR
027900051228     C   38              Z-ADD     0433239       VABCCM
028000051228     C   38              Z-ADD     0433239       VATCCM
028100051228     C   38              Z-ADD     043           VABLNP
028200051228     C   38              Z-ADD     043           VATLNP
028300051228     C   38              Z-ADD     001           VABCTR
028400101012     C   39              Z-ADD     1110743       VABCCM
028500101012     C   39              Z-ADD     1110743       VATCCM
028600110221     C   39              Z-ADD     166           VABLNP
028700110221     C   39              Z-ADD     166           VATLNP
028800101012     C   39              Z-ADD     001           VABCTR
028900120213     C*
029000120213     C   40              Z-ADD     0021744       VABCCM
029100120213     C   40              Z-ADD     0021744       VATCCM
029200120213     C   40              Z-ADD     002           VABLNP
029300120213     C   40              Z-ADD     002           VATLNP
029400120213     C   40              Z-ADD     001           VABCTR
029500120213     C   41              Z-ADD     0220905       VABCCM
029600120213     C   41              Z-ADD     0220905       VATCCM
029700120213     C   41              Z-ADD     022           VABLNP
029800120213     C   41              Z-ADD     022           VATLNP
029900120213     C   41              Z-ADD     001           VABCTR
030000120213     C   42              Z-ADD     0939669       VABCCM
030100120213     C   42              Z-ADD     0939669       VATCCM
030200120213     C   42              Z-ADD     093           VABLNP
030300120213     C   42              Z-ADD     093           VATLNP
030400120213     C   42              Z-ADD     001           VABCTR
030500120213     C   43              Z-ADD     0471329       VABCCM
030600120213     C   43              Z-ADD     0471329       VATCCM
030700120213     C   43              Z-ADD     047           VABLNP
030800120213     C   43              Z-ADD     047           VATLNP
030900120213     C   43              Z-ADD     001           VABCTR
031000120213     C   44              Z-ADD     0063055       VABCCM
031100120213     C   44              Z-ADD     0063055       VATCCM
031200120213     C   44              Z-ADD     006           VABLNP
031300120213     C   44              Z-ADD     006           VATLNP
031400120213     C   44              Z-ADD     001           VABCTR
031500120213     C   45              Z-ADD     0982240       VABCCM
031600120213     C   45              Z-ADD     0982240       VATCCM
031700120213     C   45              Z-ADD     098           VABLNP
031800120213     C   45              Z-ADD     098           VATLNP
031900120213     C   45              Z-ADD     001           VABCTR
032000120213     C   46              Z-ADD     0400290       VABCCM
032100120213     C   46              Z-ADD     0400290       VATCCM
032200120213     C   46              Z-ADD     040           VABLNP
032300120213     C   46              Z-ADD     040           VATLNP
032400120213     C   46              Z-ADD     001           VABCTR
032500120213     C   47              Z-ADD     0250618       VABCCM
032600120213     C   47              Z-ADD     0250618       VATCCM
032700120213     C   47              Z-ADD     025           VABLNP
032800120213     C   47              Z-ADD     025           VATLNP
032900120213     C   47              Z-ADD     001           VABCTR
033000120213     C   48              Z-ADD     2242028       VABCCM
033100120213     C   48              Z-ADD     2242028       VATCCM
033200120213     C   48              Z-ADD     224           VABLNP
033300120213     C   48              Z-ADD     224           VATLNP
033400120213     C   48              Z-ADD     001           VABCTR
033500120213     C   49              Z-ADD     0661774       VABCCM
033600120213     C   49              Z-ADD     0661774       VATCCM
033700120213     C   49              Z-ADD     066           VABLNP
033800120213     C   49              Z-ADD     066           VATLNP
033900120213     C   49              Z-ADD     001           VABCTR
034000120213     C   50              Z-ADD     1230516       VABCCM
034100120213     C   50              Z-ADD     1230516       VATCCM
034200120213     C   50              Z-ADD     123           VABLNP
034300120213     C   50              Z-ADD     123           VATLNP
034400120213     C   50              Z-ADD     001           VABCTR
034500120720     C   51              Z-ADD     0203097       VABCCM
034600120720     C   51              Z-ADD     0203097       VATCCM
034700120926     C***51              Z-ADD     223           VABLNP
034800120926     C***51              Z-ADD     223           VATLNP
034900120926     C   51              Z-ADD     020           VABLNP
035000120926     C   51              Z-ADD     020           VATLNP
035100120720     C   51              Z-ADD     000           VABCTR
035200121030     C   52              Z-ADD     0952024       VABCCM
035300121030     C   52              Z-ADD     0952024       VATCCM
035400121030     C   52              Z-ADD     095           VABLNP
035500121030     C   52              Z-ADD     095           VATLNP
035600121030     C   52              Z-ADD     001           VABCTR
035700051228     C*
035800051206     C                   MOVEL     '7Q'          VABCTM
035900051206     C                   MOVEL     '1'           VABCBO
036000020619     C* ... e poi verifico se sono stati passati come parametri
036100020619     C                   IF        vlrppt > *blanks
036200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
036300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036400020619     C                   EXSR      CHKNUM
036500020619     C                   IF        PiInt=*on
036600020619     C                   Z-ADD     PiVal         VABCCM
036700020619     C                   Z-ADD     PiVal         VATCCM
036800180220     C                   Z-ADD     PiVal         VADCCM
036900020619     C                   ENDIF
037000040506     C                   ENDIF
037100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
037200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037300020619     C                   EXSR      CHKNUM
037400020619     C                   IF        PiInt=*on
037500020619     C                   Z-ADD     PiVal         VABLNP
037600020619     C                   Z-ADD     PiVal         VATLNP
037700180220     C                   Z-ADD     PiVal         VADLNP
037800040506     C                   ENDIF
037900020619     C                   ENDIF
038000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
038100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038200020619     C                   EXSR      CHKNUM
038300020619     C                   IF        PiInt=*on
038400020619     C                   Z-ADD     PiVal         VABCTR
038500040506     C                   ENDIF
038600020619     C                   ENDIF
038700041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
038800041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
038900041210     C                   ENDIF
039000020619     C                   ENDIF
039100000801     C*
039200000801     C                   ENDSR
039300000801     C*----------------------------------------------------*
039400040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
039500000801     C*----------------------------------------------------*
039600040910     C     IMPVABVAT     BEGSR
039700041210     C*
039800041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
039900041210     C                   Z-ADD     *zeros        errore            1 0
040000051213     C*
040100051213     C********
040200051213     C* Tipo record 'VIA' (riconoscimento cliente mittente)
040300051213     C********
040400051213     C                   IF        %trim(%subst(vindta:1:3)) = 'VIA'
040500051228     C                   SETOFF                                       202122
040600051228     C                   SETOFF                                       232425
040700051228     C                   SETOFF                                       262728
040800051228     C                   SETOFF                                       293031
040900051228     C                   SETOFF                                       323334
041000051228     C                   SETOFF                                       353637
041100101012     C                   SETOFF                                       3839
041200120213     C                   SETOFF                                       404142
041300120213     C                   SETOFF                                       434445
041400120213     C                   SETOFF                                       464748
041500120720     C                   SETOFF                                       495051
041600121030     C                   SETOFF                                       52
041700080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
041800051228     C                             %trim(%subst(vindta:116:5)) = 'BOSCH'
041900051228     C                   SETON                                        20
042000051213     C                   ENDIF
042100080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
042200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042300051228     C                   SETON                                        21
042400051228     C                   ENDIF
042500080109     C                   IF        %trim(%subst(vindta:54:2)) = '17' AND
042600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042700051228     C                   SETON                                        22
042800051228     C                   ENDIF
042900080109     C                   IF        %trim(%subst(vindta:54:2)) = '11' AND
043000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043100051228     C                   SETON                                        23
043200051228     C                   ENDIF
043300080109     C                   IF        %trim(%subst(vindta:54:2)) = '10' AND
043400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043500051228     C                   SETON                                        24
043600051228     C                   ENDIF
043700080109     C                   IF        %trim(%subst(vindta:54:2)) = '08' AND
043800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043900051228     C                   SETON                                        25
044000051228     C                   ENDIF
044100080109     C                   IF        %trim(%subst(vindta:54:2)) = '04' AND
044200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044300051228     C                   SETON                                        26
044400051228     C                   ENDIF
044500080109     C                   IF        %trim(%subst(vindta:54:2)) = '02' AND
044600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044700051228     C                   SETON                                        27
044800051228     C                   ENDIF
044900080109     C                   IF        %trim(%subst(vindta:54:2)) = '03'      AND
045000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045100051228     C                   SETON                                        29
045200051228     C                   ENDIF
045300080109     C                   IF        %trim(%subst(vindta:54:2)) = '05'      AND
045400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045500051228     C                   SETON                                        31
045600051228     C                   ENDIF
045700100223     C                   IF        %trim(%subst(vindta:54:2)) = '34'      AND
045800100223     C                             %trim(%subst(vindta:116:5)) = *blanks
045900100223     C                   SETON                                        34
046000100223     C                   ENDIF
046100080109     C                   IF        %trim(%subst(vindta:54:2)) = '12'      AND
046200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
046300051228     C                   SETON                                        35
046400051228     C                   ENDIF
046500080109     C                   IF        %trim(%subst(vindta:54:2)) = '13'      AND
046600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
046700051228     C                   SETON                                        36
046800051228     C                   ENDIF
046900080109     C                   IF        %trim(%subst(vindta:54:2)) = '18'      AND
047000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
047100051228     C                   SETON                                        38
047200051228     C                   ENDIF
047300110607     C***                IF        %trim(%subst(vindta:54:2)) = '06'
047400110607     C                   IF        %trim(%subst(vindta:54:2)) = '75'
047500101012     C                   SETON                                        39
047600101012     C                   ENDIF
047700120213     C                   IF        %trim(%subst(vindta:54:2)) = '10'
047800120213     C                   SETON                                        40
047900120213     C                   ENDIF
048000120213     C                   IF        %trim(%subst(vindta:54:2)) = '36'
048100120213     C                   SETON                                        41
048200120213     C                   ENDIF
048300120213     C                   IF        %trim(%subst(vindta:54:2)) = '05'
048400120213     C                   SETON                                        42
048500120213     C                   ENDIF
048600120213     C                   IF        %trim(%subst(vindta:54:2)) = '72'
048700120213     C                   SETON                                        43
048800120213     C                   ENDIF
048900120213     C                   IF        %trim(%subst(vindta:54:2)) = '38'
049000120213     C                   SETON                                        44
049100120213     C                   ENDIF
049200120213     C                   IF        %trim(%subst(vindta:54:2)) = '20'
049300120213     C                   SETON                                        45
049400120213     C                   ENDIF
049500120213     C                   IF        %trim(%subst(vindta:54:2)) = '23'
049600120213     C                   SETON                                        46
049700120213     C                   ENDIF
049800120213     C                   IF        %trim(%subst(vindta:54:2)) = '09'
049900120213     C                   SETON                                        47
050000120213     C                   ENDIF
050100120213     C                   IF        %trim(%subst(vindta:54:2)) = '08'
050200120213     C                   SETON                                        48
050300120213     C                   ENDIF
050400120213     C                   IF        %trim(%subst(vindta:54:2)) = '02'
050500120213     C                   SETON                                        49
050600120213     C                   ENDIF
050700120213     C                   IF        %trim(%subst(vindta:54:2)) = '15'
050800120213     C                   SETON                                        50
050900120213     C                   ENDIF
051000120720     C                   IF        %trim(%subst(vindta:54:2)) = '36'
051100120720     C                   SETON                                        51
051200120720     C                   ENDIF
051300121030     C                   IF        %trim(%subst(vindta:54:2)) = '37'
051400121030     C                   SETON                                        52
051500121030     C                   ENDIF
051600101012     C                   ENDIF
051700051213     C********
051800051213     C* FINE Tipo record 'VIA'
051900051213     C********
052000040910     C*
052100041210     C********
052200051206     C* Tipo record 'PR1' (riferimenti, dati destinatario, importi e particolarità consegna)
052300041210     C********
052400051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR1'
052500041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
052600041210     C                   IF        FlgNewBol = '1'
052700041210     C                   WRITE     FIVAB000
052800041210     C                   EVAL      FlgNewBol = '0'
052900041210     C                   ENDIF
053000041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
053100040910     C                   EXSR      INZVAR
053200040910     C                   EXSR      DEFCAM
053300041210     C                   EVAL      FlgNewBol = '1'
053400040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
053500040928     C                   MOVEL     datcor        VABAAS
053600040928     C                   MOVEL     datcor        VATAAS
053700040928     C                   MOVE      datcor        VABMGS
053800051228     C                   MOVE(P)   VABLNP        VABFGS
053900051228     C                   MOVE(P)   VABLNP        VATFGS
054000180219     C                   MOVE(P)   VABLNP        VADFGS
054100180219     C                   MOVEL     datcor        VADAAS
054200051206     C* ......VABNSP/VATNSP/VABRMN/VABRMA
054300051206     C                   EVAL      VABRMA=%trim(%subst(vindta:4:27))
054400051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
054500051206     C                   EXSR      CHKNUM
054600051206     C                   IF        PiInt=*on
054700051206     C                   Z-ADD     PiVal         VABRMN
054800051206     C                   ELSE
054900051206     C                   ADD       1             errore
055000051206     C                   EVAL      vinmsg = %trimr(vinmsg)
055100051206     C                             + ' ' + 'VABRMN'
055200051206     C                   ENDIF
055300051206     C*
055400051206     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
055500051206     C                   EXSR      CHKNUM
055600051206     C                   IF        PiInt=*on
055700051206     C                   Z-ADD     PiVal         VABNSP
055800051206     C                   Z-ADD     PiVal         VATNSP
055900180219     C                   Z-ADD     PiVal         VADNSP
056000041210     C                   ELSE
056100041210     C                   ADD       1             errore
056200041210     C                   EVAL      vinmsg = %trimr(vinmsg)
056300180219     C                             + ' ' + 'VABNSP VATNSP'
056400041210     C                   ENDIF
056500051206     C* ......VABRMO
056600051206     C                   IF        %trim(%subst(vindta:46:35)) <>
056700051206     C                             'GEODIS ZUST AMBROSETTI'
056800051206     C                   EVAL      VABRMO=%trim(%subst(vindta:46:35))
056900051206     C                   ENDIF
057000051206     C* ......VATNOT + VATTRC = 'B'
057100051206     C                   IF        %trim(%subst(vindta:272:35)) <> *blanks
057200051206     C                   EVAL      VATNOT=%trim(%subst(vindta:272:35))
057300051206     C                   EVAL      VATTRC = 'B'
057400051206     C                   WRITE     FIVAT000
057500051206     C                   ENDIF
057600051206     C* ......VABRSD
057700051206     C                   EVAL      VABRSD=%trim(%subst(vindta:428:35))
057800050805     C* ......VABIND
057900051206     C                   EVAL      VABIND=%trim(%subst(vindta:498:35))
058000050805     C* ......VABLOD
058100051206     C                   EVAL      VABLOD=%trim(%subst(vindta:578:35))
058200050805     C* ......VABCAD
058300051206     C                   EVAL      PiStr=%trim(%subst(vindta:568:10))
058400050805     C                   EXSR      CHKNUM
058500050805     C                   IF        PiInt=*on
058600050805     C                   Z-ADD     PiVal         Num5_0
058700050805     C                   MOVEL(P)  Num5_0        VABCAD
058800050805     C                   ELSE
058900050805     C                   ADD       1             errore
059000050805     C                   EVAL      vinmsg = %trimr(vinmsg)
059100050805     C                             + ' ' + 'VABCAD'
059200050805     C                   ENDIF
059300051206     C* ......VABPRD
059400051206     C                   EVAL      VABPRD=%trim(%subst(vindta:613:3))
059500051206     C* ......VABCBO
059600051206     C                   IF        %trim(%subst(vindta:619:3)) = 'DDU'
059700051206     C                   EVAL      VABCBO='1'
059800051206     C                   ENDIF
059900051206     C                   IF        %trim(%subst(vindta:619:3)) = 'EXW'
060000051206     C                   EVAL      VABCBO='2'
060100051206     C                   ENDIF
060200051206     C* ......VABCAS/VABVCA
060300051206     C                   IF        %trim(%subst(vindta:628:15)) <> *zeros
060400051206     C                   EVAL      FlgCAS = '1'
060500051206     C*
060600051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBK'
060700051206     C                   EVAL      VABTIC='OM'
060800051206     C                   ENDIF
060900051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBC'
061000051206     C                   EVAL      VABTIC='OM'
061100051206     C                   ENDIF
061200051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YCB'
061300051206     C                   EVAL      VABTIC='OC'
061400051206     C                   ENDIF
061500051206     C                   IF        %trim(%subst(vindta:643:3)) = 'CSH'
061600051206     C                   EVAL      VABTIC=*blanks
061700051206     C                   ENDIF
061800051206     C*
061900051206     C                   EVAL      VABVCA=%trim(%subst(vindta:625:3))
062000051206     C*
062100051206     C                   EVAL      PiStr=%trim(%subst(vindta:628:15))
062200051206     C                   EXSR      CHKNUM
062300051206     C                   IF        PiNum=*on
062400051206     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
062500051206     C                   Z-ADD     PiVal         VABCAS
062600051206     C                   ELSE
062700051206     C                   ADD       1             errore
062800051206     C                   EVAL      vinmsg = %trimr(vinmsg)
062900051206     C                             + ' ' + 'VABCAS'
063000051206     C                   ENDIF
063100051206     C                   ENDIF
063200180219     C*
063300180219     C* Dati esclusivi SOLO per alcuni clienti
063400180219     C                   SELECT
063500180219     C                   WHEN      VABCCM = 2230606
063600180219     C* ......VATNOT + VATTRC = 'P'
063700180219     C                   IF        %trim(%subst(vindta:856:17)) <> *blanks
063800180219     C                   EVAL      VATNOT=%trim(%subst(vindta:856:17))
063900180219     C                   EVAL      %subst(VATNOT:34:2)='10'
064000180219     C                   EVAL      VATTRC = 'P'
064100180219     C                   WRITE     FIVAT000
064200180219     C                   ENDIF
064300180219     C* ......VABNRS
064400180219     C                   EVAL      PiStr=%trim(%subst(vindta:873:2))
064500180219     C                   EXSR      CHKNUM
064600180219     C                   IF        PiInt=*on
064700180219     C                   Z-ADD     PiVal         VABNRS
064800180219     C                   Z-ADD     PiVal         VATNRS
064900180219     C                   Z-ADD     PiVal         VADNRS
065000180219     C                   ELSE
065100180219     C                   ADD       1             errore
065200180219     C                   EVAL      vinmsg = %trimr(vinmsg)
065300180219     C                             + ' ' + 'VABNRS VATNRS VADNRS'
065400180219     C                   ENDIF
065500180219     C* ......VABLNA
065600180219     C                   EVAL      PiStr=%trim(%subst(vindta:875:3))
065700180219     C                   EXSR      CHKNUM
065800180219     C                   IF        PiInt=*on
065900180219     C                   Z-ADD     PiVal         VABLNA
066000180219     C                   ELSE
066100180219     C                   ADD       1             errore
066200180219     C                   EVAL      vinmsg = %trimr(vinmsg)
066300180219     C                             + ' ' + 'VABLNA'
066400180219     C                   ENDIF
066500180219     C* ......VABZNC
066600180219     C                   EVAL      PiStr=%trim(%subst(vindta:878:2))
066700180219     C                   EXSR      CHKNUM
066800180219     C                   IF        PiInt=*on
066900180219     C                   Z-ADD     PiVal         VABZNC
067000180219     C                   ELSE
067100180219     C                   ADD       1             errore
067200180219     C                   EVAL      vinmsg = %trimr(vinmsg)
067300180219     C                             + ' ' + 'VABZNC'
067400180219     C                   ENDIF
067500180219     C*
067600180219     C                   ENDSL
067700180219     C*
067800060307     C* Considerazioni sul contenuto di campi precedentemente valorizzati
067900060307     C                   IF        FlgCAS <> '0'
068000060307     C                   IF        VABCBO = '1'
068100060307     C                   EVAL      VABCBO = '4'
068200060307     C                   ELSE
068300060307     C                   EVAL      VABCBO = '6'
068400060307     C                   ENDIF
068500060307     C                   ENDIF
068600051206     C*
068700051206     C                   ENDIF
068800051206     C********
068900051206     C* FINE Tipo record 'PR1'
069000051206     C********
069100051206     C*
069200051206     C********
069300051206     C* Tipo record 'PR2' (note, consegna richiesta, valore da assicurare)
069400051206     C********
069500051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR2'
069600051206     C* ......VABDCR
069700051213     C                   IF        %subst(vindta:7:8)<>*blanks
069800051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
069900050805     C                   EXSR      CHKNUM
070000051206     C                   IF        PiInt=*on
070100051206     C                   Z-ADD     PiVal         VABDCR
070200050805     C                   ELSE
070300050805     C                   ADD       1             errore
070400050805     C                   EVAL      vinmsg = %trimr(vinmsg)
070500051206     C                             + ' ' + 'VABDCR'
070600050805     C                   ENDIF
070700051213     C                   ENDIF
070800051206     C* ......VABHCR
070900051213     C                   IF        %subst(vindta:7+8:4)<>*blanks
071000051206     C                   EVAL      PiStr=%trim(%subst(vindta:7+8:4))
071100051206     C                   EXSR      CHKNUM
071200051206     C                   IF        PiInt=*on
071300051206     C                   Z-ADD     PiVal         VABHCR
071400051206     C                   ELSE
071500051206     C                   ADD       1             errore
071600051206     C                   EVAL      vinmsg = %trimr(vinmsg)
071700051206     C                             + ' ' + 'VABHCR'
071800051206     C                   ENDIF
071900051213     C                   ENDIF
072000051206     C* ......VABNOT
072100051206     C                   EVAL      VABNOT=%trim(%subst(vindta:22:35))
072200051206     C* ......VABNT2
072300051206     C                   EVAL      VABNT2=%trim(%subst(vindta:22+35:35))
072400051206     C* ......VABIAS/VABVAS
072500051206     C                   IF        %subst(vindta:845:1) = 'S'
072600051206     C                   EVAL      VABVAS=%trim(%subst(vindta:846:3))
072700051206     C                   EVAL      PiStr=%trim(%subst(vindta:849:15))
072800051206     C                   EXSR      CHKNUM
072900051206     C                   IF        PiNum=*on
073000051206     C                   EVAL      PiVal = PiVal / 100
073100051206     C                   Z-ADD     PiVal         VABIAS
073200051206     C                   ELSE
073300051206     C                   ADD       1             errore
073400051206     C                   EVAL      vinmsg = %trimr(vinmsg)
073500051206     C                             + ' ' + 'VABIAS'
073600051206     C                   ENDIF
073700051206     C                   ENDIF
073800051206     C*
073900051206     C                   ENDIF
074000051206     C********
074100051206     C* FINE Tipo record 'PR2'
074200051206     C********
074300051206     C*
074400051206     C********
074500051206     C* Tipo record 'MER' (colli, peso)
074600051206     C********
074700051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MER'
074800050805     C* ......VABNCL
074900051206     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
075000050805     C                   EXSR      CHKNUM
075100050805     C                   IF        PiInt=*on
075200080208     C***                Z-ADD     PiVal         VABNCL
075300080208     C                   ADD       PiVal         VABNCL
075400050805     C                   ELSE
075500050805     C                   ADD       1             errore
075600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
075700050805     C                             + ' ' + 'VABNCL'
075800050805     C                   ENDIF
075900050805     C* ......VABPKB
076000051206     C                   EVAL      PiStr=%trim(%subst(vindta:15:10))
076100050805     C                   EXSR      CHKNUM
076200050805     C                   IF        PiNum=*on
076300051206     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
076400080208     C***                Z-ADD     PiVal         VABPKB
076500080208     C                   ADD       PiVal         VABPKB
076600050805     C                   ELSE
076700050805     C                   ADD       1             errore
076800050805     C                   EVAL      vinmsg = %trimr(vinmsg)
076900050805     C                             + ' ' + 'VABPKB'
077000050805     C                   ENDIF
077100061012     C* ......VABVLB
077200061012     C                   EVAL      PiStr=%trim(%subst(vindta:35:10))
077300061012     C                   EXSR      CHKNUM
077400061012     C                   IF        PiNum=*on
077500061012     C                   EVAL      PiVal = PiVal / 1000                         * da decim3 a metri3
077600080208     C***                Z-ADD     PiVal         VABVLB
077700080208     C                   ADD       PiVal         VABVLB
077800061012     C                   ELSE
077900061012     C                   ADD       1             errore
078000061012     C                   EVAL      vinmsg = %trimr(vinmsg)
078100061012     C                             + ' ' + 'VABVLB'
078200061012     C                   ENDIF
078300050805     C*
078400050805     C                   ENDIF
078500051206     C********
078600051206     C* FINE Tipo record 'MER'
078700051206     C********
078800051206     C*
078900051206     C********
079000051206     C* Tipo record 'MAR' (barcode)
079100051206     C********
079200051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MAR'
079300180219     C* e non è il tipo rcd MAR24 che non ha il barcode ma il collo
079400180219     C                             and %trim(%subst(vindta:1:5)) <>'MAR24'
079500051206     C* ......VATNOT + VATTRC = 'E'
079600051213     C                   IF        %trim(%subst(vindta:7:20)) <> *blanks
079700051213     C                   EVAL      VATNOT=%trim(%subst(vindta:7:20))
079800051206     C                   EVAL      VATTRC = 'E'
079900051206     C                   WRITE     FIVAT000
080000051206     C                   ENDIF
080100050805     C*
080200050805     C                   ENDIF
080300051206     C********
080400051206     C* FINE Tipo record 'MAR'
080500051206     C********
080600180219     C*
080700180219     C********
080800180219     C* Tipo record 'MAR24' (marcatura alternativa)
080900180219     C********
081000180219     C                   IF        %trim(%subst(vindta:1:5)) = 'MAR24'
081100180219     C* devo essere in sol.B2
081200180219     C                             and %subst(vlrppt:16:2) = 'B2'
081300180219     C* ......VADNCD
081400180219     C                   IF        %trim(%subst(vindta:50:7)) <> *blanks
081500180219     C                   EVAL      PiStr=%trim(%subst(vindta:50:7))
081600180219     C                   EXSR      CHKNUM
081700180219     C                   IF        PiInt=*on
081800180219     C                   Z-ADD     PiVal         VADNCD
081900180220     C                   WRITE     FIVAD000
082000180219     C                   ELSE
082100180219     C                   ADD       1             errore
082200180219     C                   EVAL      vinmsg = %trimr(vinmsg)
082300180219     C                             + ' ' + 'VADNCD'
082400180219     C                   ENDIF
082500180219     C                   ENDIF
082600180219     C*
082700180219     C                   ENDIF
082800180219     C********
082900180219     C* FINE Tipo record 'MAR'
083000180219     C********
083100040910     C*
083200040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
083300040910     C                   EXSR      CHKIMPDIV
083400010202     C*
083500000801     C* Ebbene...
083600000801     C                   ADD       1             §CTRMO
083700010201     C                   IF        errore <> *zeros
083800000801     C                   ADD       1             §CTRNO
083900000801     C                   EVAL      vinflg = '2'
084000000801     C                   ELSE
084100010201     C                   ADD       1             §CTROKVB
084200000801     C                   ENDIF
084300000801     C*
084400000801     C                   ENDSR
084500010202     C*----------------------------------------------------*
084600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
084700010202     C*----------------------------------------------------*
084800020305     C     PREVAT        BEGSR
084900010202     C*
085000021113     C* Compongo il nome del membro da dare al FIVATWWR
085100010202     C                   eval      parmbr = vlrhdl
085200010202     C                   movel     'M'           parmbr
085300041210     C                   eval      parccm = vlrksc
085400010202     C                   eval      paropz = '1'
085500010202     C* Effettuo la chiamata al CLLE preposto
085600180220     C                   SELECT
085700180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
085800180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
085900180220     C                   call(e)   'TITVVDTC'
086000180220     C                   parm                    parccm
086100180220     C                   parm                    parmbr
086200180220     C                   parm                    paropz
086300180220     C                   OTHER
086400180220     C                   call(e)   'TITVVTC'
086500180220     C                   parm                    parccm
086600180220     C                   parm                    parmbr
086700180220     C                   parm                    paropz
086800180220     C                   ENDSL
086900180220     C*
087000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
087100010202     C                   if        %error
087200010202     C                   movel     '1'           chkcall
087300010202     C                   else
087400010202     C                   movel     '0'           chkcall
087500010202     C                   endif
087600010202     C*
087700010202     C                   ENDSR
087800000801     C*----------------------------------------------------*
087900000801     C*  CONTROLLO NUMERICITA' CAMPI
088000000801     C*----------------------------------------------------*
088100000801     C     CHKNUM        BEGSR
088200000801     C*
088300000801     C                   call(e)   'ISNUMERIC'
088400000801     C                   PARM                    PiStr            30
088500041210     C                   PARM      '.'           PiDecChr          1
088600000801     C                   PARM      *ZEROS        PiVal            30 9
088700000801     C                   PARM      '0'           PiInt             1
088800000801     C                   PARM      '0'           PiNum             1
088900000801     C                   IF        %error
089000000801     C                   EVAL      PiInt=*off
089100000801     C                   ENDIF
089200000801     C*
089300000801     C                   ENDSR
089400000801     C***
089500000801
089600011113
089700011113     C*----------------------------------------------------*
089800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
089900011113     C*----------------------------------------------------*
090000011113     C     CHKIMPDIV     BEGSR
090100011113     C*
090200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
090300011113     C                   Z-ADD     *zeros        wrkDec            9 9
090400011113     C*
090500011113     C* Come prima cosa effettuo considerazioni sulla divisa
090600011113     C                   IF        vabIAS > *zeros
090700011113     C                   IF        vabVAS <> 'EUR'
090800011113     C                   EVAL      vabVAS =  'ITL'
090900011113     C                   ENDIF
091000011113     C                   ENDIF
091100011113     C*
091200011113     C                   IF        vabCAS > *zeros
091300011113     C                   IF        vabVCA <> 'EUR'
091400011113     C                   EVAL      vabVCA =  'ITL'
091500011113     C                   ENDIF
091600011113     C                   ENDIF
091700011113     C*
091800011113     C                   IF        vabVMD > *zeros
091900020305     C                   IF        vabVAD <> 'EUR'
092000011113     C                   EVAL      vabVAD =  'ITL'
092100011113     C                   ENDIF
092200011113     C                   ENDIF
092300011113     C*
092400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
092500011113     C                   Z-ADD     vabIAS        wrkDec
092600011113     C                   IF        wrkDec > *zeros
092700011113     C                   IF        vabVAS = 'ITL'
092800011113     C                   EVAL      vabIAS = *zeros
092900011113     C                   ENDIF
093000011113     C                   ENDIF
093100011113     C*
093200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
093300011113     C                   Z-ADD     vabCAS        wrkDec
093400011113     C                   IF        wrkDec > *zeros
093500011113     C                   IF        vabVCA = 'ITL'
093600011113     C                   EVAL      vabCAS = *zeros
093700011113     C                   ENDIF
093800011113     C                   ENDIF
093900011113     C*
094000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
094100011113     C                   Z-ADD     vabVMD        wrkDec
094200011113     C                   IF        wrkDec > *zeros
094300011113     C                   IF        vabVAD = 'ITL'
094400011113     C                   EVAL      vabVMD = *zeros
094500011113     C                   ENDIF
094600011113     C                   ENDIF
094700011113     C*
094800011113     C                   ENDSR
094900011113     C***
095000011113
095100011113
095200000801
095300000801
095400990920      /TITLE Invio dei dati al punto operativo.
095500010202     C     invio         BEGSR
095600990920     C*
095700180220     C* 1° invio FIVAD
095800180220     C* solo se è richiesta sol. B2 (dettaglio colli non sequeziale)
095900180220     C                   IF        %subst(vlrppt:16:2) = 'B2'
096000010201     C                   reset                   dscmz
096100180220     C                   move      VADLNP        cmzdst
096200180220     C                   eval      cmzfld = 'FIVADWWR'
096300010201     C                   eval      cmzmbd = vlrhdl
096400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
096500021009     C***                if        prmfir = *blanks
096600180220     C                   eval      cmzfla = 'FIVAD00F'
096700180220     C                   eval      cmzmba = 'FIVAD00F'
096800021009     C***                else
096900021009     C***                eval      cmzfla = prmfir
097000021009     C***                eval      cmzmba = prmfir
097100021009     C***                endif
097200010201     C                   eval      cmznrr = *zeros
097300020305     C                   move      §ctrokvt      cmznrr
097400021018     C                   eval      cmzlba = vlrfl1
097500010201     C                   call(e)   'TIS711C'
097600010201     C                   parm                    dscmz
097700010201     C                   parm      *blanks       esito
097800010205     C                   if        %error
097900010205     C                             or cmzerr = '1'
098000010205     C                             or esito  = '1'
098100010205     C                   eval      wrkesito = '3'
098200180220     C                   endif
098300180220     C                   endif
098400180220     C*
098500180220     C* 2° invio FIVAT
098600180220     C* (solo se ho inviato VAD c'è la possibilità che wrkesito sia 3=errore)
098700180220     C                   IF        wrkesito <> '3'
098800180220     C                   reset                   dscmz
098900180220     C                   move      VATLNP        cmzdst
099000180220     C                   eval      cmzfld = 'FIVATWWR'
099100180220     C                   eval      cmzmbd = vlrhdl
099200180220     C                   eval      %subst(cmzmbd:1:1) = 'M'
099300180220     C***                if        prmfir = *blanks
099400180220     C                   eval      cmzfla = 'FIVAT00F'
099500180220     C                   eval      cmzmba = 'FIVAT00F'
099600180220     C***                else
099700180220     C***                eval      cmzfla = prmfir
099800180220     C***                eval      cmzmba = prmfir
099900180220     C***                endif
100000180220     C                   eval      cmznrr = *zeros
100100180220     C                   move      §ctrokvt      cmznrr
100200180220     C                   eval      cmzlba = vlrfl1
100300180220     C                   call(e)   'TIS711C'
100400180220     C                   parm                    dscmz
100500180220     C                   parm      *blanks       esito
100600180220     C                   if        %error
100700180220     C                             or cmzerr = '1'
100800180220     C                             or esito  = '1'
100900180220     C                   eval      wrkesito = '3'
101000180220     C                   else
101100010201     C*
101200180220     C* 3° invio FIVAB
101300010201     C                   reset                   dscmz
101400051228     C                   move      VABLNP        cmzdst
101500010201     C                   eval      cmzfld = vlrfou
101600010201     C                   eval      cmzmbd = vlrhdl
101700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
101800021009     C***                if        prmfir = *blanks
101900021113     C                   eval      cmzfla = 'FIVAB00F'
102000021113     C                   eval      cmzmba = 'FIVAB00F'
102100021009     C***                else
102200021009     C***                eval      cmzfla = prmfir
102300021009     C***                eval      cmzmba = prmfir
102400021009     C***                endif
102500010201     C                   eval      cmznrr = *zeros
102600010201     C                   move      §ctrokvb      cmznrr
102700021018     C                   eval      cmzlba = vlrfl1
102800010201     C                   call(e)   'TIS711C'
102900010201     C                   parm                    dscmz
103000010201     C                   parm      *blanks       esito
103100010201     C                   if        %error
103200010201     C                             or cmzerr = '1'
103300010201     C                             or esito  = '1'
103400010201     C                   eval      wrkesito = '3'
103500010201     C                   endif
103600010205     C                   endif
103700180220     C                   endif
103800990920     C*
103900000613     C                   ENDSR
104000000613     C***
104100070411
104200070411     C     *pssr         BEGSR
104300070411     C*
104400070411     C                   if        %open(tivin00r)
104500070411     C                   close     tivin00r
104600070411     C                   endif
104700070411     C                   if        %open(fivabwwr)
104800070411     C                   close     fivabwwr
104900070411     C                   endif
105000070411     C                   if        %open(fivatwwr)
105100070411     C                   close     fivatwwr
105200070411     C                   endif
105300180219     C                   if        %open(fivadwwr)
105400180219     C                   close     fivadwwr
105500180219     C                   endif
105600070411     C*
105700070411     C* Effettuo la chiamata al CLLE preposto
105800180220     C                   SELECT
105900180220     C* è richiesta sol. B2 (dettaglio colli non sequeziale)
106000180220     C                   WHEN      %subst(vlrppt:16:2) = 'B2'
106100180220     C                   call(e)   'TITVVDTC'
106200180220     C                   parm                    parccm
106300180220     C                   parm                    parmbr
106400180220     C                   parm      '2'           paropz
106500180220     C                   OTHER
106600180220     C                   call(e)   'TITVVTC'
106700180220     C                   parm                    parccm
106800180220     C                   parm                    parmbr
106900180220     C                   parm      '2'           paropz
107000180220     C                   ENDSL
107100070411     C*
107200070411     C                   eval      wrkesito = '2'
107300070411     C*
107400070411     C                   seton                                        LR
107500070411     C*
107600070411     C                   ENDSR     '*CANCL'
107700070411     C***
107800070411
107900990910
108000000613     C     *inzsr        BEGSR
108100990910     C*
108200990910     C     *entry        plist
108300990920     C                   parm                    tivlrds
108400990921     C                   parm      wrkesito      esito
108500000724     C                   parm                    prmlit
108600000710     C                   parm                    prmfir
108700000613     C*
108800000830     C* CALCOLA LA DATA CORRENTE
108900121030     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
109000121030     C                   eval      datcor = %dec(%date() : *ISO)
109100000830     C*
109200000613     C                   ENDSR
109300000613     C***
