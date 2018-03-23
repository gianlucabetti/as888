000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
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
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000041210     D parccm          s              8    INZ(*blanks)
002100010202     D parmbr          s             10    INZ(*blanks)
002200010202     D paropz          s              1    INZ(*blanks)
002300010202     D chkcall         s              1    INZ(*blanks)
002400051206     D FlgNewBol       s              1    INZ(*blanks)
002500010201
002600010201
002700000913     C                   reset                   rrnum
002800990921     C                   reset                   esito
002900990921     C                   reset                   wrkesito
003000051228     C*
003100051228     C                   movel     '0'           wOVRPRTF          1
003200000613     C*
003300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
003400000613     C*
003500010202     C* Effettuo la chiamata al CLLE preposto
003600040506     C                   call(e)   'TITVVTC'
003700010202     C                   parm                    parccm
003800010202     C                   parm                    parmbr
003900010202     C                   parm      '2'           paropz
004000000616     C*
004100010201     C                   seton                                        LR
004200041210
004300041210
004400041210
004500910830     C*--------------------------------------------------------
004600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
004700910830     C*--------------------------------------------------------
004800040526     C     RWFILE        BEGSR
004900990910     C*
005000990914     C                   if        not %open(tivin00r)
005100990908     C                   open      tivin00r
005200990914     C                   endif
005300021113     C                   if        not %open(fivabwwr)
005400021113     C                   open      fivabwwr
005500990914     C                   endif
005600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
005700020305     C                   exsr      prevat
005800010201     C*
005900010202     C                   if        chkcall = '0'
006000010202     C*
006100021113     C                   if        not %open(fivatwwr)
006200021113     C                   open      fivatwwr
006300010201     C                   endif
006400990910     C*
006500010201     C                   clear                   §CTROKVB          5 0
006600020305     C                   clear                   §CTROKVT          5 0
006700000801     C                   clear                   §CTRMO            5 0
006800000801     C                   clear                   §CTRNO            5 0
006900040910     C*
007000921023     C                   DO        *HIVAL
007100990913     C*
007200990915     C                   READ      tivin00r                               70
007300040910     C                   if        vindta > *blanks
007400000613     C                   add       1             rrnum
007500000801     C*
007600000801     C                   if        *in70 = *off
007700000801     C                             and
007800000801     C                             (vinflg = *blanks
007900000801     C                              or vinflg = '0'
008000000801     C                              or vinflg = '2')
008100000801     C*
008200000801     C                   clear                   vinmsg
008300000801     C                   eval      vinflg = '1'
008400040910     C*
008500040910     C* Eseguo routine d traduzione
008600040910     C                   exsr      impvabvat
008700040802     C*
008800010305     C                   endif
008900000905     C*
009000000905     C                   else
009100000905     C                   eval      vinflg = '1'
009200000905     C                   endif
009300000905     C*
009400000905     C  N70              update    tivin000
009500000905     C*
009600991022     C  N70              ENDdo
009700041210     C*
009800050805     C* Scarico i buffer eventualmente rimasti in sospeso
009900041210     C                   IF        FlgNewBol = '1'
010000041210     C                   WRITE     FIVAB000
010100041210     C                   ENDIF
010200010202     C*
010300010202     C                   endif
010400990910
010500990910     C* Se non ci sono record con errori ...
010600000710     C                   if        §ctrno = 0
010700990910     C* ... restituisco esito OK.
010800990921     C                   eval      wrkesito = '0'
010900990910     C                   else
011000010201     C                   if        §ctrokvb > 0
011100990921     C                   eval      wrkesito = '1'
011200000710     C                   else
011300000710     C                   eval      wrkesito = '2'
011400990910     C                   endif
011500000710     C                   endif
011600990910     C*
011700990914     C                   if        %open(tivin00r)
011800990908     C                   close     tivin00r
011900990914     C                   endif
012000021113     C                   if        %open(fivabwwr)
012100021113     C                   close     fivabwwr
012200990914     C                   endif
012300021113     C                   if        %open(fivatwwr)
012400021113     C                   close     fivatwwr
012500010201     C                   endif
012600990910     C*
012700010201     C                   if        §ctrokvb > 0
012800000724     C                             and vlrpoi <> *zeros
012900010202     C                   exsr      invio
013000990920     C                   endif
013100990920     C*
013200910830     C                   ENDSR
013300000613     C***
013400990920
013500000801     C*----------------------------------------------------*
013600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
013700000801     C*----------------------------------------------------*
013800010201     C     INZVAR        BEGSR
013900000801     C*
014000040802     C                   Z-ADD     *zeros        Num5_0            5 0
014100051206     C                   MOVEL     '0'           FlgCAS            1
014200000801     C*
014300000801     C                   ENDSR
014400000801     C*----------------------------------------------------*
014500040910     C*  IMPOSTAZIONE CAMPI COSTANTI
014600000801     C*----------------------------------------------------*
014700000801     C     DEFCAM        BEGSR
014800000801     C*
014900021113     C                   CLEAR                   FIVAB000
015000040802     C                   CLEAR                   FIVAT000
015100051228     C* Imposto i valori di default...
015200051228     C                   Z-ADD     0053039       VABCCM
015300051228     C                   Z-ADD     0053039       VATCCM
015400051228     C                   Z-ADD     005           VABLNP
015500051228     C                   Z-ADD     005           VATLNP
015600090317     C                   Z-ADD     000           VABCTR
015700051228     C* Traffici IMPORT
015800051228     C   20              Z-ADD     0053578       VABCCM
015900051228     C   20              Z-ADD     0053578       VATCCM
016000051228     C   20              Z-ADD     005           VABLNP
016100051228     C   20              Z-ADD     005           VATLNP
016200051228     C   20              Z-ADD     000           VABCTR
016300051228     C   21              Z-ADD     0053039       VABCCM
016400051228     C   21              Z-ADD     0053039       VATCCM
016500051228     C   21              Z-ADD     005           VABLNP
016600051228     C   21              Z-ADD     005           VATLNP
016700090317     C   21              Z-ADD     000           VABCTR
016800060825     C*  22              Z-ADD     0493248       VABCCM
016900060825     C*  22              Z-ADD     0493248       VATCCM
017000060825     C   22              Z-ADD     1190493       VABCCM
017100060825     C   22              Z-ADD     1190493       VATCCM
017200060307     C   22              Z-ADD     119           VABLNP
017300060307     C   22              Z-ADD     119           VATLNP
017400051228     C   22              Z-ADD     001           VABCTR
017500100223 xxx C***23              Z-ADD     0650826       VABCCM
017600100223 xxx C***23              Z-ADD     0650826       VATCCM
017700100223     C   23              Z-ADD     0650945       VABCCM
017800100223     C   23              Z-ADD     0650945       VATCCM
017900051228     C   23              Z-ADD     065           VABLNP
018000051228     C   23              Z-ADD     065           VATLNP
018100051228     C   23              Z-ADD     001           VABCTR
018200051228     C   24              Z-ADD     0021744       VABCCM
018300051228     C   24              Z-ADD     0021744       VATCCM
018400051228     C   24              Z-ADD     002           VABLNP
018500051228     C   24              Z-ADD     002           VATLNP
018600051228     C   24              Z-ADD     001           VABCTR
018700120110     C   25              Z-ADD     2242028       VABCCM
018800120110     C   25              Z-ADD     2242028       VATCCM
018900120110     C   25              Z-ADD     224           VABLNP
019000120110     C   25              Z-ADD     224           VATLNP
019100051228     C   25              Z-ADD     001           VABCTR
019200051228     C   26              Z-ADD     0012821       VABCCM
019300051228     C   26              Z-ADD     0012821       VATCCM
019400051228     C   26              Z-ADD     001           VABLNP
019500051228     C   26              Z-ADD     001           VATLNP
019600051228     C   26              Z-ADD     001           VABCTR
019700051228     C   27              Z-ADD     1280515       VABCCM
019800051228     C   27              Z-ADD     1280515       VATCCM
019900051228     C   27              Z-ADD     128           VABLNP
020000051228     C   27              Z-ADD     128           VATLNP
020100051228     C   27              Z-ADD     001           VABCTR
020200051228     C* Traffici NAZIONALI
020300051228     C   28              Z-ADD     1280515       VABCCM
020400051228     C   28              Z-ADD     1280515       VATCCM
020500051228     C   28              Z-ADD     128           VABLNP
020600051228     C   28              Z-ADD     128           VATLNP
020700051228     C   28              Z-ADD     001           VABCTR
020800051228     C   29              Z-ADD     0502928       VABCCM
020900051228     C   29              Z-ADD     0502928       VATCCM
021000051228     C   29              Z-ADD     050           VABLNP
021100051228     C   29              Z-ADD     050           VATLNP
021200051228     C   29              Z-ADD     001           VABCTR
021300051228     C   30              Z-ADD     0012821       VABCCM
021400051228     C   30              Z-ADD     0012821       VATCCM
021500051228     C   30              Z-ADD     001           VABLNP
021600051228     C   30              Z-ADD     001           VATLNP
021700051228     C   30              Z-ADD     001           VABCTR
021800051228     C   31              Z-ADD     0939669       VABCCM
021900051228     C   31              Z-ADD     0939669       VATCCM
022000051228     C   31              Z-ADD     093           VABLNP
022100051228     C   31              Z-ADD     093           VATLNP
022200051228     C   31              Z-ADD     001           VABCTR
022300051228     C   32              Z-ADD     0840647       VABCCM
022400051228     C   32              Z-ADD     0840647       VATCCM
022500051228     C   32              Z-ADD     084           VABLNP
022600051228     C   32              Z-ADD     084           VATLNP
022700051228     C   32              Z-ADD     001           VABCTR
022800051228     C   33              Z-ADD     0021744       VABCCM
022900051228     C   33              Z-ADD     0021744       VATCCM
023000051228     C   33              Z-ADD     002           VABLNP
023100051228     C   33              Z-ADD     002           VATLNP
023200051228     C   33              Z-ADD     001           VABCTR
023300100223     C***34              Z-ADD     0650826       VABCCM
023400100223     C***34              Z-ADD     0650826       VATCCM
023500120608     C***34              Z-ADD     0202104       VABCCM
023600120608     C***34              Z-ADD     0202104       VATCCM
023700120608     C   34              Z-ADD     0203067       VABCCM
023800120608     C   34              Z-ADD     0203067       VATCCM
023900100223     C   34              Z-ADD     020           VABLNP
024000100223     C   34              Z-ADD     020           VATLNP
024100051228     C   34              Z-ADD     001           VABCTR
024200051228     C   35              Z-ADD     0063055       VABCCM
024300051228     C   35              Z-ADD     0063055       VATCCM
024400051228     C   35              Z-ADD     006           VABLNP
024500051228     C   35              Z-ADD     006           VATLNP
024600051228     C   35              Z-ADD     001           VABCTR
024700051228     C   36              Z-ADD     0201370       VABCCM
024800051228     C   36              Z-ADD     0201370       VATCCM
024900051228     C   36              Z-ADD     020           VABLNP
025000051228     C   36              Z-ADD     020           VATLNP
025100051228     C   36              Z-ADD     001           VABCTR
025200060825     C*  37              Z-ADD     0493248       VABCCM
025300060825     C*  37              Z-ADD     0493248       VATCCM
025400060825     C   37              Z-ADD     1190493       VABCCM
025500060825     C   37              Z-ADD     1190493       VATCCM
025600060307     C   37              Z-ADD     119           VABLNP
025700060307     C   37              Z-ADD     119           VATLNP
025800051228     C   37              Z-ADD     001           VABCTR
025900051228     C   38              Z-ADD     0433239       VABCCM
026000051228     C   38              Z-ADD     0433239       VATCCM
026100051228     C   38              Z-ADD     043           VABLNP
026200051228     C   38              Z-ADD     043           VATLNP
026300051228     C   38              Z-ADD     001           VABCTR
026400101012     C   39              Z-ADD     1110743       VABCCM
026500101012     C   39              Z-ADD     1110743       VATCCM
026600110221     C   39              Z-ADD     166           VABLNP
026700110221     C   39              Z-ADD     166           VATLNP
026800101012     C   39              Z-ADD     001           VABCTR
026900120213     C*
027000120213     C   40              Z-ADD     0021744       VABCCM
027100120213     C   40              Z-ADD     0021744       VATCCM
027200120213     C   40              Z-ADD     002           VABLNP
027300120213     C   40              Z-ADD     002           VATLNP
027400120213     C   40              Z-ADD     001           VABCTR
027500120213     C   41              Z-ADD     0220905       VABCCM
027600120213     C   41              Z-ADD     0220905       VATCCM
027700120213     C   41              Z-ADD     022           VABLNP
027800120213     C   41              Z-ADD     022           VATLNP
027900120213     C   41              Z-ADD     001           VABCTR
028000120213     C   42              Z-ADD     0939669       VABCCM
028100120213     C   42              Z-ADD     0939669       VATCCM
028200120213     C   42              Z-ADD     093           VABLNP
028300120213     C   42              Z-ADD     093           VATLNP
028400120213     C   42              Z-ADD     001           VABCTR
028500120213     C   43              Z-ADD     0471329       VABCCM
028600120213     C   43              Z-ADD     0471329       VATCCM
028700120213     C   43              Z-ADD     047           VABLNP
028800120213     C   43              Z-ADD     047           VATLNP
028900120213     C   43              Z-ADD     001           VABCTR
029000120213     C   44              Z-ADD     0063055       VABCCM
029100120213     C   44              Z-ADD     0063055       VATCCM
029200120213     C   44              Z-ADD     006           VABLNP
029300120213     C   44              Z-ADD     006           VATLNP
029400120213     C   44              Z-ADD     001           VABCTR
029500120213     C   45              Z-ADD     0982240       VABCCM
029600120213     C   45              Z-ADD     0982240       VATCCM
029700120213     C   45              Z-ADD     098           VABLNP
029800120213     C   45              Z-ADD     098           VATLNP
029900120213     C   45              Z-ADD     001           VABCTR
030000120213     C   46              Z-ADD     0400290       VABCCM
030100120213     C   46              Z-ADD     0400290       VATCCM
030200120213     C   46              Z-ADD     040           VABLNP
030300120213     C   46              Z-ADD     040           VATLNP
030400120213     C   46              Z-ADD     001           VABCTR
030500120213     C   47              Z-ADD     0250618       VABCCM
030600120213     C   47              Z-ADD     0250618       VATCCM
030700120213     C   47              Z-ADD     025           VABLNP
030800120213     C   47              Z-ADD     025           VATLNP
030900120213     C   47              Z-ADD     001           VABCTR
031000120213     C   48              Z-ADD     2242028       VABCCM
031100120213     C   48              Z-ADD     2242028       VATCCM
031200120213     C   48              Z-ADD     224           VABLNP
031300120213     C   48              Z-ADD     224           VATLNP
031400120213     C   48              Z-ADD     001           VABCTR
031500120213     C   49              Z-ADD     0661774       VABCCM
031600120213     C   49              Z-ADD     0661774       VATCCM
031700120213     C   49              Z-ADD     066           VABLNP
031800120213     C   49              Z-ADD     066           VATLNP
031900120213     C   49              Z-ADD     001           VABCTR
032000120213     C   50              Z-ADD     1230516       VABCCM
032100120213     C   50              Z-ADD     1230516       VATCCM
032200120213     C   50              Z-ADD     123           VABLNP
032300120213     C   50              Z-ADD     123           VATLNP
032400120213     C   50              Z-ADD     001           VABCTR
032500120720     C   51              Z-ADD     0203097       VABCCM
032600120720     C   51              Z-ADD     0203097       VATCCM
032700120926     C***51              Z-ADD     223           VABLNP
032800120926     C***51              Z-ADD     223           VATLNP
032900120926     C   51              Z-ADD     020           VABLNP
033000120926     C   51              Z-ADD     020           VATLNP
033100120720     C   51              Z-ADD     000           VABCTR
033200121030     C   52              Z-ADD     0952024       VABCCM
033300121030     C   52              Z-ADD     0952024       VATCCM
033400121030     C   52              Z-ADD     095           VABLNP
033500121030     C   52              Z-ADD     095           VATLNP
033600121030     C   52              Z-ADD     001           VABCTR
033700180308     c* Nuovo traffico dalla 166
033800180308     C   53              Z-ADD     1664129       VABCCM
033900180308     C   53              Z-ADD     1664129       VATCCM
034000180308     C   53              Z-ADD     166           VABLNP
034100180308     C   53              Z-ADD     166           VATLNP
034200180308     C   53              Z-ADD     399           VABCTR
034300051228     C*
034400051206     C                   MOVEL     '7Q'          VABCTM
034500051206     C                   MOVEL     '1'           VABCBO
034600020619     C* ... e poi verifico se sono stati passati come parametri
034700020619     C                   IF        vlrppt > *blanks
034800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
034900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035000020619     C                   EXSR      CHKNUM
035100020619     C                   IF        PiInt=*on
035200020619     C                   Z-ADD     PiVal         VABCCM
035300020619     C                   Z-ADD     PiVal         VATCCM
035400020619     C                   ENDIF
035500040506     C                   ENDIF
035600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
035700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035800020619     C                   EXSR      CHKNUM
035900020619     C                   IF        PiInt=*on
036000020619     C                   Z-ADD     PiVal         VABLNP
036100020619     C                   Z-ADD     PiVal         VATLNP
036200040506     C                   ENDIF
036300020619     C                   ENDIF
036400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
036500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036600020619     C                   EXSR      CHKNUM
036700020619     C                   IF        PiInt=*on
036800020619     C                   Z-ADD     PiVal         VABCTR
036900040506     C                   ENDIF
037000020619     C                   ENDIF
037100041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
037200041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
037300041210     C                   ENDIF
037400020619     C                   ENDIF
037500000801     C*
037600000801     C                   ENDSR
037700000801     C*----------------------------------------------------*
037800040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
037900000801     C*----------------------------------------------------*
038000040910     C     IMPVABVAT     BEGSR
038100041210     C*
038200041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
038300041210     C                   Z-ADD     *zeros        errore            1 0
038400051213     C*
038500051213     C********
038600051213     C* Tipo record 'VIA' (riconoscimento cliente mittente)
038700051213     C********
038800051213     C                   IF        %trim(%subst(vindta:1:3)) = 'VIA'
038900051228     C                   SETOFF                                       202122
039000051228     C                   SETOFF                                       232425
039100051228     C                   SETOFF                                       262728
039200051228     C                   SETOFF                                       293031
039300051228     C                   SETOFF                                       323334
039400051228     C                   SETOFF                                       353637
039500101012     C                   SETOFF                                       3839
039600120213     C                   SETOFF                                       404142
039700120213     C                   SETOFF                                       434445
039800120213     C                   SETOFF                                       464748
039900120720     C                   SETOFF                                       495051
040000180308     C                   SETOFF                                       5253
040100080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
040200051228     C                             %trim(%subst(vindta:116:5)) = 'BOSCH'
040300051228     C                   SETON                                        20
040400051213     C                   ENDIF
040500080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
040600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
040700051228     C                   SETON                                        21
040800051228     C                   ENDIF
040900080109     C                   IF        %trim(%subst(vindta:54:2)) = '17' AND
041000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
041100051228     C                   SETON                                        22
041200051228     C                   ENDIF
041300080109     C                   IF        %trim(%subst(vindta:54:2)) = '11' AND
041400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
041500051228     C                   SETON                                        23
041600051228     C                   ENDIF
041700080109     C                   IF        %trim(%subst(vindta:54:2)) = '10' AND
041800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
041900051228     C                   SETON                                        24
042000051228     C                   ENDIF
042100080109     C                   IF        %trim(%subst(vindta:54:2)) = '08' AND
042200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042300051228     C                   SETON                                        25
042400051228     C                   ENDIF
042500080109     C                   IF        %trim(%subst(vindta:54:2)) = '04' AND
042600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042700051228     C                   SETON                                        26
042800051228     C                   ENDIF
042900080109     C                   IF        %trim(%subst(vindta:54:2)) = '02' AND
043000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043100051228     C                   SETON                                        27
043200051228     C                   ENDIF
043300080109     C                   IF        %trim(%subst(vindta:54:2)) = '03'      AND
043400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043500051228     C                   SETON                                        29
043600051228     C                   ENDIF
043700080109     C                   IF        %trim(%subst(vindta:54:2)) = '05'      AND
043800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043900051228     C                   SETON                                        31
044000051228     C                   ENDIF
044100100223     C                   IF        %trim(%subst(vindta:54:2)) = '34'      AND
044200100223     C                             %trim(%subst(vindta:116:5)) = *blanks
044300100223     C                   SETON                                        34
044400100223     C                   ENDIF
044500080109     C                   IF        %trim(%subst(vindta:54:2)) = '12'      AND
044600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044700051228     C                   SETON                                        35
044800051228     C                   ENDIF
044900080109     C                   IF        %trim(%subst(vindta:54:2)) = '13'      AND
045000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045100051228     C                   SETON                                        36
045200051228     C                   ENDIF
045300080109     C                   IF        %trim(%subst(vindta:54:2)) = '18'      AND
045400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
045500051228     C                   SETON                                        38
045600051228     C                   ENDIF
045700110607     C***                IF        %trim(%subst(vindta:54:2)) = '06'
045800110607     C                   IF        %trim(%subst(vindta:54:2)) = '75'
045900101012     C                   SETON                                        39
046000101012     C                   ENDIF
046100120213     C                   IF        %trim(%subst(vindta:54:2)) = '10'
046200120213     C                   SETON                                        40
046300120213     C                   ENDIF
046400120213     C                   IF        %trim(%subst(vindta:54:2)) = '36'
046500120213     C                   SETON                                        41
046600120213     C                   ENDIF
046700120213     C                   IF        %trim(%subst(vindta:54:2)) = '05'
046800120213     C                   SETON                                        42
046900120213     C                   ENDIF
047000120213     C                   IF        %trim(%subst(vindta:54:2)) = '72'
047100120213     C                   SETON                                        43
047200120213     C                   ENDIF
047300120213     C                   IF        %trim(%subst(vindta:54:2)) = '38'
047400120213     C                   SETON                                        44
047500120213     C                   ENDIF
047600120213     C                   IF        %trim(%subst(vindta:54:2)) = '20'
047700120213     C                   SETON                                        45
047800120213     C                   ENDIF
047900120213     C                   IF        %trim(%subst(vindta:54:2)) = '23'
048000120213     C                   SETON                                        46
048100120213     C                   ENDIF
048200120213     C                   IF        %trim(%subst(vindta:54:2)) = '09'
048300120213     C                   SETON                                        47
048400120213     C                   ENDIF
048500120213     C                   IF        %trim(%subst(vindta:54:2)) = '08'
048600120213     C                   SETON                                        48
048700120213     C                   ENDIF
048800120213     C                   IF        %trim(%subst(vindta:54:2)) = '02'
048900120213     C                   SETON                                        49
049000120213     C                   ENDIF
049100120213     C                   IF        %trim(%subst(vindta:54:2)) = '15'
049200120213     C                   SETON                                        50
049300120213     C                   ENDIF
049400120720     C                   IF        %trim(%subst(vindta:54:2)) = '36'
049500120720     C                   SETON                                        51
049600120720     C                   ENDIF
049700121030     C                   IF        %trim(%subst(vindta:54:2)) = '37'
049800121030     C                   SETON                                        52
049900121030     C                   ENDIF
050000180308     c* nuove codice cliente della 166
050100180308     C                   IF        %trim(%subst(vindta:54:2)) = '14'
050200180308     C                   SETON                                        53
050300180308     C                   ENDIF
050400101012     C                   ENDIF
050500051213     C********
050600051213     C* FINE Tipo record 'VIA'
050700051213     C********
050800040910     C*
050900041210     C********
051000051206     C* Tipo record 'PR1' (riferimenti, dati destinatario, importi e particolarità consegna)
051100041210     C********
051200051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR1'
051300041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
051400041210     C                   IF        FlgNewBol = '1'
051500041210     C                   WRITE     FIVAB000
051600041210     C                   EVAL      FlgNewBol = '0'
051700041210     C                   ENDIF
051800041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
051900040910     C                   EXSR      INZVAR
052000040910     C                   EXSR      DEFCAM
052100041210     C                   EVAL      FlgNewBol = '1'
052200040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
052300040928     C                   MOVEL     datcor        VABAAS
052400040928     C                   MOVEL     datcor        VATAAS
052500040928     C                   MOVE      datcor        VABMGS
052600051228     C                   MOVE(P)   VABLNP        VABFGS
052700051228     C                   MOVE(P)   VABLNP        VATFGS
052800051206     C* ......VABNSP/VATNSP/VABRMN/VABRMA
052900051206     C                   EVAL      VABRMA=%trim(%subst(vindta:4:27))
053000051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
053100051206     C                   EXSR      CHKNUM
053200051206     C                   IF        PiInt=*on
053300051206     C                   Z-ADD     PiVal         VABRMN
053400051206     C                   ELSE
053500051206     C                   ADD       1             errore
053600051206     C                   EVAL      vinmsg = %trimr(vinmsg)
053700051206     C                             + ' ' + 'VABRMN'
053800051206     C                   ENDIF
053900051206     C*
054000051206     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
054100051206     C                   EXSR      CHKNUM
054200051206     C                   IF        PiInt=*on
054300051206     C                   Z-ADD     PiVal         VABNSP
054400051206     C                   Z-ADD     PiVal         VATNSP
054500041210     C                   ELSE
054600041210     C                   ADD       1             errore
054700041210     C                   EVAL      vinmsg = %trimr(vinmsg)
054800051206     C                             + ' ' + 'VABNSP VATNSP'
054900041210     C                   ENDIF
055000051206     C* ......VABRMO
055100051206     C                   IF        %trim(%subst(vindta:46:35)) <>
055200051206     C                             'GEODIS ZUST AMBROSETTI'
055300051206     C                   EVAL      VABRMO=%trim(%subst(vindta:46:35))
055400051206     C                   ENDIF
055500051206     C* ......VATNOT + VATTRC = 'B'
055600051206     C                   IF        %trim(%subst(vindta:272:35)) <> *blanks
055700051206     C                   EVAL      VATNOT=%trim(%subst(vindta:272:35))
055800051206     C                   EVAL      VATTRC = 'B'
055900051206     C                   WRITE     FIVAT000
056000051206     C                   ENDIF
056100051206     C* ......VABRSD
056200051206     C                   EVAL      VABRSD=%trim(%subst(vindta:428:35))
056300050805     C* ......VABIND
056400051206     C                   EVAL      VABIND=%trim(%subst(vindta:498:35))
056500050805     C* ......VABLOD
056600051206     C                   EVAL      VABLOD=%trim(%subst(vindta:578:35))
056700050805     C* ......VABCAD
056800051206     C                   EVAL      PiStr=%trim(%subst(vindta:568:10))
056900050805     C                   EXSR      CHKNUM
057000050805     C                   IF        PiInt=*on
057100050805     C                   Z-ADD     PiVal         Num5_0
057200050805     C                   MOVEL(P)  Num5_0        VABCAD
057300050805     C                   ELSE
057400050805     C                   ADD       1             errore
057500050805     C                   EVAL      vinmsg = %trimr(vinmsg)
057600050805     C                             + ' ' + 'VABCAD'
057700050805     C                   ENDIF
057800051206     C* ......VABPRD
057900051206     C                   EVAL      VABPRD=%trim(%subst(vindta:613:3))
058000051206     C* ......VABCBO
058100051206     C                   IF        %trim(%subst(vindta:619:3)) = 'DDU'
058200051206     C                   EVAL      VABCBO='1'
058300051206     C                   ENDIF
058400051206     C                   IF        %trim(%subst(vindta:619:3)) = 'EXW'
058500051206     C                   EVAL      VABCBO='2'
058600051206     C                   ENDIF
058700051206     C* ......VABCAS/VABVCA
058800051206     C                   IF        %trim(%subst(vindta:628:15)) <> *zeros
058900051206     C                   EVAL      FlgCAS = '1'
059000051206     C*
059100051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBK'
059200051206     C                   EVAL      VABTIC='OM'
059300051206     C                   ENDIF
059400051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBC'
059500051206     C                   EVAL      VABTIC='OM'
059600051206     C                   ENDIF
059700051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YCB'
059800051206     C                   EVAL      VABTIC='OC'
059900051206     C                   ENDIF
060000051206     C                   IF        %trim(%subst(vindta:643:3)) = 'CSH'
060100051206     C                   EVAL      VABTIC=*blanks
060200051206     C                   ENDIF
060300051206     C*
060400051206     C                   EVAL      VABVCA=%trim(%subst(vindta:625:3))
060500051206     C*
060600051206     C                   EVAL      PiStr=%trim(%subst(vindta:628:15))
060700051206     C                   EXSR      CHKNUM
060800051206     C                   IF        PiNum=*on
060900051206     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
061000051206     C                   Z-ADD     PiVal         VABCAS
061100051206     C                   ELSE
061200051206     C                   ADD       1             errore
061300051206     C                   EVAL      vinmsg = %trimr(vinmsg)
061400051206     C                             + ' ' + 'VABCAS'
061500051206     C                   ENDIF
061600051206     C                   ENDIF
061700060307     C*
061800060307     C* Considerazioni sul contenuto di campi precedentemente valorizzati
061900060307     C                   IF        FlgCAS <> '0'
062000060307     C                   IF        VABCBO = '1'
062100060307     C                   EVAL      VABCBO = '4'
062200060307     C                   ELSE
062300060307     C                   EVAL      VABCBO = '6'
062400060307     C                   ENDIF
062500060307     C                   ENDIF
062600051206     C*
062700051206     C                   ENDIF
062800051206     C********
062900051206     C* FINE Tipo record 'PR1'
063000051206     C********
063100051206     C*
063200051206     C********
063300051206     C* Tipo record 'PR2' (note, consegna richiesta, valore da assicurare)
063400051206     C********
063500051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR2'
063600051206     C* ......VABDCR
063700051213     C                   IF        %subst(vindta:7:8)<>*blanks
063800051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
063900050805     C                   EXSR      CHKNUM
064000051206     C                   IF        PiInt=*on
064100051206     C                   Z-ADD     PiVal         VABDCR
064200050805     C                   ELSE
064300050805     C                   ADD       1             errore
064400050805     C                   EVAL      vinmsg = %trimr(vinmsg)
064500051206     C                             + ' ' + 'VABDCR'
064600050805     C                   ENDIF
064700051213     C                   ENDIF
064800051206     C* ......VABHCR
064900051213     C                   IF        %subst(vindta:7+8:4)<>*blanks
065000051206     C                   EVAL      PiStr=%trim(%subst(vindta:7+8:4))
065100051206     C                   EXSR      CHKNUM
065200051206     C                   IF        PiInt=*on
065300051206     C                   Z-ADD     PiVal         VABHCR
065400051206     C                   ELSE
065500051206     C                   ADD       1             errore
065600051206     C                   EVAL      vinmsg = %trimr(vinmsg)
065700051206     C                             + ' ' + 'VABHCR'
065800051206     C                   ENDIF
065900051213     C                   ENDIF
066000051206     C* ......VABNOT
066100051206     C                   EVAL      VABNOT=%trim(%subst(vindta:22:35))
066200051206     C* ......VABNT2
066300051206     C                   EVAL      VABNT2=%trim(%subst(vindta:22+35:35))
066400051206     C* ......VABIAS/VABVAS
066500051206     C                   IF        %subst(vindta:845:1) = 'S'
066600051206     C                   EVAL      VABVAS=%trim(%subst(vindta:846:3))
066700051206     C                   EVAL      PiStr=%trim(%subst(vindta:849:15))
066800051206     C                   EXSR      CHKNUM
066900051206     C                   IF        PiNum=*on
067000051206     C                   EVAL      PiVal = PiVal / 100
067100051206     C                   Z-ADD     PiVal         VABIAS
067200051206     C                   ELSE
067300051206     C                   ADD       1             errore
067400051206     C                   EVAL      vinmsg = %trimr(vinmsg)
067500051206     C                             + ' ' + 'VABIAS'
067600051206     C                   ENDIF
067700051206     C                   ENDIF
067800051206     C*
067900051206     C                   ENDIF
068000051206     C********
068100051206     C* FINE Tipo record 'PR2'
068200051206     C********
068300051206     C*
068400051206     C********
068500051206     C* Tipo record 'MER' (colli, peso)
068600051206     C********
068700051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MER'
068800050805     C* ......VABNCL
068900051206     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
069000050805     C                   EXSR      CHKNUM
069100050805     C                   IF        PiInt=*on
069200080208     C***                Z-ADD     PiVal         VABNCL
069300080208     C                   ADD       PiVal         VABNCL
069400050805     C                   ELSE
069500050805     C                   ADD       1             errore
069600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
069700050805     C                             + ' ' + 'VABNCL'
069800050805     C                   ENDIF
069900050805     C* ......VABPKB
070000051206     C                   EVAL      PiStr=%trim(%subst(vindta:15:10))
070100050805     C                   EXSR      CHKNUM
070200050805     C                   IF        PiNum=*on
070300051206     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
070400080208     C***                Z-ADD     PiVal         VABPKB
070500080208     C                   ADD       PiVal         VABPKB
070600050805     C                   ELSE
070700050805     C                   ADD       1             errore
070800050805     C                   EVAL      vinmsg = %trimr(vinmsg)
070900050805     C                             + ' ' + 'VABPKB'
071000050805     C                   ENDIF
071100061012     C* ......VABVLB
071200061012     C                   EVAL      PiStr=%trim(%subst(vindta:35:10))
071300061012     C                   EXSR      CHKNUM
071400061012     C                   IF        PiNum=*on
071500061012     C                   EVAL      PiVal = PiVal / 1000                         * da decim3 a metri3
071600080208     C***                Z-ADD     PiVal         VABVLB
071700080208     C                   ADD       PiVal         VABVLB
071800061012     C                   ELSE
071900061012     C                   ADD       1             errore
072000061012     C                   EVAL      vinmsg = %trimr(vinmsg)
072100061012     C                             + ' ' + 'VABVLB'
072200061012     C                   ENDIF
072300050805     C*
072400050805     C                   ENDIF
072500051206     C********
072600051206     C* FINE Tipo record 'MER'
072700051206     C********
072800051206     C*
072900051206     C********
073000051206     C* Tipo record 'MAR' (barcode)
073100051206     C********
073200051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MAR'
073300051206     C* ......VATNOT + VATTRC = 'E'
073400051213     C                   IF        %trim(%subst(vindta:7:20)) <> *blanks
073500051213     C                   EVAL      VATNOT=%trim(%subst(vindta:7:20))
073600051206     C                   EVAL      VATTRC = 'E'
073700051206     C                   WRITE     FIVAT000
073800051206     C                   ENDIF
073900050805     C*
074000050805     C                   ENDIF
074100051206     C********
074200051206     C* FINE Tipo record 'MAR'
074300051206     C********
074400040910     C*
074500040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
074600040910     C                   EXSR      CHKIMPDIV
074700010202     C*
074800000801     C* Ebbene...
074900000801     C                   ADD       1             §CTRMO
075000010201     C                   IF        errore <> *zeros
075100000801     C                   ADD       1             §CTRNO
075200000801     C                   EVAL      vinflg = '2'
075300000801     C                   ELSE
075400010201     C                   ADD       1             §CTROKVB
075500000801     C                   ENDIF
075600000801     C*
075700000801     C                   ENDSR
075800010202     C*----------------------------------------------------*
075900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
076000010202     C*----------------------------------------------------*
076100020305     C     PREVAT        BEGSR
076200010202     C*
076300021113     C* Compongo il nome del membro da dare al FIVATWWR
076400010202     C                   eval      parmbr = vlrhdl
076500010202     C                   movel     'M'           parmbr
076600041210     C                   eval      parccm = vlrksc
076700010202     C                   eval      paropz = '1'
076800010202     C* Effettuo la chiamata al CLLE preposto
076900040506     C                   call(e)   'TITVVTC'
077000010202     C                   parm                    parccm
077100010202     C                   parm                    parmbr
077200010202     C                   parm                    paropz
077300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
077400010202     C                   if        %error
077500010202     C                   movel     '1'           chkcall
077600010202     C                   else
077700010202     C                   movel     '0'           chkcall
077800010202     C                   endif
077900010202     C*
078000010202     C                   ENDSR
078100000801     C*----------------------------------------------------*
078200000801     C*  CONTROLLO NUMERICITA' CAMPI
078300000801     C*----------------------------------------------------*
078400000801     C     CHKNUM        BEGSR
078500000801     C*
078600000801     C                   call(e)   'ISNUMERIC'
078700000801     C                   PARM                    PiStr            30
078800041210     C                   PARM      '.'           PiDecChr          1
078900000801     C                   PARM      *ZEROS        PiVal            30 9
079000000801     C                   PARM      '0'           PiInt             1
079100000801     C                   PARM      '0'           PiNum             1
079200000801     C                   IF        %error
079300000801     C                   EVAL      PiInt=*off
079400000801     C                   ENDIF
079500000801     C*
079600000801     C                   ENDSR
079700000801     C***
079800000801
079900011113
080000011113     C*----------------------------------------------------*
080100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
080200011113     C*----------------------------------------------------*
080300011113     C     CHKIMPDIV     BEGSR
080400011113     C*
080500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
080600011113     C                   Z-ADD     *zeros        wrkDec            9 9
080700011113     C*
080800011113     C* Come prima cosa effettuo considerazioni sulla divisa
080900011113     C                   IF        vabIAS > *zeros
081000011113     C                   IF        vabVAS <> 'EUR'
081100011113     C                   EVAL      vabVAS =  'ITL'
081200011113     C                   ENDIF
081300011113     C                   ENDIF
081400011113     C*
081500011113     C                   IF        vabCAS > *zeros
081600011113     C                   IF        vabVCA <> 'EUR'
081700011113     C                   EVAL      vabVCA =  'ITL'
081800011113     C                   ENDIF
081900011113     C                   ENDIF
082000011113     C*
082100011113     C                   IF        vabVMD > *zeros
082200020305     C                   IF        vabVAD <> 'EUR'
082300011113     C                   EVAL      vabVAD =  'ITL'
082400011113     C                   ENDIF
082500011113     C                   ENDIF
082600011113     C*
082700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
082800011113     C                   Z-ADD     vabIAS        wrkDec
082900011113     C                   IF        wrkDec > *zeros
083000011113     C                   IF        vabVAS = 'ITL'
083100011113     C                   EVAL      vabIAS = *zeros
083200011113     C                   ENDIF
083300011113     C                   ENDIF
083400011113     C*
083500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
083600011113     C                   Z-ADD     vabCAS        wrkDec
083700011113     C                   IF        wrkDec > *zeros
083800011113     C                   IF        vabVCA = 'ITL'
083900011113     C                   EVAL      vabCAS = *zeros
084000011113     C                   ENDIF
084100011113     C                   ENDIF
084200011113     C*
084300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
084400011113     C                   Z-ADD     vabVMD        wrkDec
084500011113     C                   IF        wrkDec > *zeros
084600011113     C                   IF        vabVAD = 'ITL'
084700011113     C                   EVAL      vabVMD = *zeros
084800011113     C                   ENDIF
084900011113     C                   ENDIF
085000011113     C*
085100011113     C                   ENDSR
085200011113     C***
085300011113
085400011113
085500000801
085600000801
085700990920      /TITLE Invio dei dati al punto operativo.
085800010202     C     invio         BEGSR
085900990920     C*
086000021113     C* 1° invio FIVAT
086100010201     C                   reset                   dscmz
086200051228     C                   move      VATLNP        cmzdst
086300021113     C                   eval      cmzfld = 'FIVATWWR'
086400010201     C                   eval      cmzmbd = vlrhdl
086500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
086600021009     C***                if        prmfir = *blanks
086700021113     C                   eval      cmzfla = 'FIVAT00F'
086800021113     C                   eval      cmzmba = 'FIVAT00F'
086900021009     C***                else
087000021009     C***                eval      cmzfla = prmfir
087100021009     C***                eval      cmzmba = prmfir
087200021009     C***                endif
087300010201     C                   eval      cmznrr = *zeros
087400020305     C                   move      §ctrokvt      cmznrr
087500021018     C                   eval      cmzlba = vlrfl1
087600010201     C                   call(e)   'TIS711C'
087700010201     C                   parm                    dscmz
087800010201     C                   parm      *blanks       esito
087900010205     C                   if        %error
088000010205     C                             or cmzerr = '1'
088100010205     C                             or esito  = '1'
088200010205     C                   eval      wrkesito = '3'
088300010205     C                   else
088400010201     C*
088500021113     C* 2° invio FIVAB
088600010201     C                   reset                   dscmz
088700051228     C                   move      VABLNP        cmzdst
088800010201     C                   eval      cmzfld = vlrfou
088900010201     C                   eval      cmzmbd = vlrhdl
089000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
089100021009     C***                if        prmfir = *blanks
089200021113     C                   eval      cmzfla = 'FIVAB00F'
089300021113     C                   eval      cmzmba = 'FIVAB00F'
089400021009     C***                else
089500021009     C***                eval      cmzfla = prmfir
089600021009     C***                eval      cmzmba = prmfir
089700021009     C***                endif
089800010201     C                   eval      cmznrr = *zeros
089900010201     C                   move      §ctrokvb      cmznrr
090000021018     C                   eval      cmzlba = vlrfl1
090100010201     C                   call(e)   'TIS711C'
090200010201     C                   parm                    dscmz
090300010201     C                   parm      *blanks       esito
090400010201     C                   if        %error
090500010201     C                             or cmzerr = '1'
090600010201     C                             or esito  = '1'
090700010201     C                   eval      wrkesito = '3'
090800010201     C                   endif
090900010205     C                   endif
091000990920     C*
091100000613     C                   ENDSR
091200000613     C***
091300070411
091400070411     C     *pssr         BEGSR
091500070411     C*
091600070411     C                   if        %open(tivin00r)
091700070411     C                   close     tivin00r
091800070411     C                   endif
091900070411     C                   if        %open(fivabwwr)
092000070411     C                   close     fivabwwr
092100070411     C                   endif
092200070411     C                   if        %open(fivatwwr)
092300070411     C                   close     fivatwwr
092400070411     C                   endif
092500070411     C*
092600070411     C* Effettuo la chiamata al CLLE preposto
092700070411     C                   call(e)   'TITVVTC'
092800070411     C                   parm                    parccm
092900070411     C                   parm                    parmbr
093000070411     C                   parm      '2'           paropz
093100070411     C*
093200070411     C                   eval      wrkesito = '2'
093300070411     C*
093400070411     C                   seton                                        LR
093500070411     C*
093600070411     C                   ENDSR     '*CANCL'
093700070411     C***
093800070411
093900990910
094000000613     C     *inzsr        BEGSR
094100990910     C*
094200990910     C     *entry        plist
094300990920     C                   parm                    tivlrds
094400990921     C                   parm      wrkesito      esito
094500000724     C                   parm                    prmlit
094600000710     C                   parm                    prmfir
094700000613     C*
094800000830     C* CALCOLA LA DATA CORRENTE
094900121030     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
095000121030     C                   eval      datcor = %dec(%date() : *ISO)
095100000830     C*
095200000613     C                   ENDSR
095300000613     C***
