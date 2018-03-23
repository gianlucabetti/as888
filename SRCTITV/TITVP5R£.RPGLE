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
033700051228     C*
033800051206     C                   MOVEL     '7Q'          VABCTM
033900051206     C                   MOVEL     '1'           VABCBO
034000020619     C* ... e poi verifico se sono stati passati come parametri
034100020619     C                   IF        vlrppt > *blanks
034200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
034300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034400020619     C                   EXSR      CHKNUM
034500020619     C                   IF        PiInt=*on
034600020619     C                   Z-ADD     PiVal         VABCCM
034700020619     C                   Z-ADD     PiVal         VATCCM
034800020619     C                   ENDIF
034900040506     C                   ENDIF
035000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
035100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035200020619     C                   EXSR      CHKNUM
035300020619     C                   IF        PiInt=*on
035400020619     C                   Z-ADD     PiVal         VABLNP
035500020619     C                   Z-ADD     PiVal         VATLNP
035600040506     C                   ENDIF
035700020619     C                   ENDIF
035800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
035900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036000020619     C                   EXSR      CHKNUM
036100020619     C                   IF        PiInt=*on
036200020619     C                   Z-ADD     PiVal         VABCTR
036300040506     C                   ENDIF
036400020619     C                   ENDIF
036500041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
036600041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
036700041210     C                   ENDIF
036800020619     C                   ENDIF
036900000801     C*
037000000801     C                   ENDSR
037100000801     C*----------------------------------------------------*
037200040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
037300000801     C*----------------------------------------------------*
037400040910     C     IMPVABVAT     BEGSR
037500041210     C*
037600041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
037700041210     C                   Z-ADD     *zeros        errore            1 0
037800051213     C*
037900051213     C********
038000051213     C* Tipo record 'VIA' (riconoscimento cliente mittente)
038100051213     C********
038200051213     C                   IF        %trim(%subst(vindta:1:3)) = 'VIA'
038300051228     C                   SETOFF                                       202122
038400051228     C                   SETOFF                                       232425
038500051228     C                   SETOFF                                       262728
038600051228     C                   SETOFF                                       293031
038700051228     C                   SETOFF                                       323334
038800051228     C                   SETOFF                                       353637
038900101012     C                   SETOFF                                       3839
039000120213     C                   SETOFF                                       404142
039100120213     C                   SETOFF                                       434445
039200120213     C                   SETOFF                                       464748
039300120720     C                   SETOFF                                       495051
039400121030     C                   SETOFF                                       52
039500080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
039600051228     C                             %trim(%subst(vindta:116:5)) = 'BOSCH'
039700051228     C                   SETON                                        20
039800051213     C                   ENDIF
039900080109     C                   IF        %trim(%subst(vindta:54:2)) = '16' AND
040000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
040100051228     C                   SETON                                        21
040200051228     C                   ENDIF
040300080109     C                   IF        %trim(%subst(vindta:54:2)) = '17' AND
040400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
040500051228     C                   SETON                                        22
040600051228     C                   ENDIF
040700080109     C                   IF        %trim(%subst(vindta:54:2)) = '11' AND
040800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
040900051228     C                   SETON                                        23
041000051228     C                   ENDIF
041100080109     C                   IF        %trim(%subst(vindta:54:2)) = '10' AND
041200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
041300051228     C                   SETON                                        24
041400051228     C                   ENDIF
041500080109     C                   IF        %trim(%subst(vindta:54:2)) = '08' AND
041600051228     C                             %trim(%subst(vindta:116:5)) = *blanks
041700051228     C                   SETON                                        25
041800051228     C                   ENDIF
041900080109     C                   IF        %trim(%subst(vindta:54:2)) = '04' AND
042000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042100051228     C                   SETON                                        26
042200051228     C                   ENDIF
042300080109     C                   IF        %trim(%subst(vindta:54:2)) = '02' AND
042400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042500051228     C                   SETON                                        27
042600051228     C                   ENDIF
042700080109     C                   IF        %trim(%subst(vindta:54:2)) = '03'      AND
042800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
042900051228     C                   SETON                                        29
043000051228     C                   ENDIF
043100080109     C                   IF        %trim(%subst(vindta:54:2)) = '05'      AND
043200051228     C                             %trim(%subst(vindta:116:5)) = *blanks
043300051228     C                   SETON                                        31
043400051228     C                   ENDIF
043500100223     C                   IF        %trim(%subst(vindta:54:2)) = '34'      AND
043600100223     C                             %trim(%subst(vindta:116:5)) = *blanks
043700100223     C                   SETON                                        34
043800100223     C                   ENDIF
043900080109     C                   IF        %trim(%subst(vindta:54:2)) = '12'      AND
044000051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044100051228     C                   SETON                                        35
044200051228     C                   ENDIF
044300080109     C                   IF        %trim(%subst(vindta:54:2)) = '13'      AND
044400051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044500051228     C                   SETON                                        36
044600051228     C                   ENDIF
044700080109     C                   IF        %trim(%subst(vindta:54:2)) = '18'      AND
044800051228     C                             %trim(%subst(vindta:116:5)) = *blanks
044900051228     C                   SETON                                        38
045000051228     C                   ENDIF
045100110607     C***                IF        %trim(%subst(vindta:54:2)) = '06'
045200110607     C                   IF        %trim(%subst(vindta:54:2)) = '75'
045300101012     C                   SETON                                        39
045400101012     C                   ENDIF
045500120213     C                   IF        %trim(%subst(vindta:54:2)) = '10'
045600120213     C                   SETON                                        40
045700120213     C                   ENDIF
045800120213     C                   IF        %trim(%subst(vindta:54:2)) = '36'
045900120213     C                   SETON                                        41
046000120213     C                   ENDIF
046100120213     C                   IF        %trim(%subst(vindta:54:2)) = '05'
046200120213     C                   SETON                                        42
046300120213     C                   ENDIF
046400120213     C                   IF        %trim(%subst(vindta:54:2)) = '72'
046500120213     C                   SETON                                        43
046600120213     C                   ENDIF
046700120213     C                   IF        %trim(%subst(vindta:54:2)) = '38'
046800120213     C                   SETON                                        44
046900120213     C                   ENDIF
047000120213     C                   IF        %trim(%subst(vindta:54:2)) = '20'
047100120213     C                   SETON                                        45
047200120213     C                   ENDIF
047300120213     C                   IF        %trim(%subst(vindta:54:2)) = '23'
047400120213     C                   SETON                                        46
047500120213     C                   ENDIF
047600120213     C                   IF        %trim(%subst(vindta:54:2)) = '09'
047700120213     C                   SETON                                        47
047800120213     C                   ENDIF
047900120213     C                   IF        %trim(%subst(vindta:54:2)) = '08'
048000120213     C                   SETON                                        48
048100120213     C                   ENDIF
048200120213     C                   IF        %trim(%subst(vindta:54:2)) = '02'
048300120213     C                   SETON                                        49
048400120213     C                   ENDIF
048500120213     C                   IF        %trim(%subst(vindta:54:2)) = '15'
048600120213     C                   SETON                                        50
048700120213     C                   ENDIF
048800120720     C                   IF        %trim(%subst(vindta:54:2)) = '36'
048900120720     C                   SETON                                        51
049000120720     C                   ENDIF
049100121030     C                   IF        %trim(%subst(vindta:54:2)) = '37'
049200121030     C                   SETON                                        52
049300121030     C                   ENDIF
049400101012     C                   ENDIF
049500051213     C********
049600051213     C* FINE Tipo record 'VIA'
049700051213     C********
049800040910     C*
049900041210     C********
050000051206     C* Tipo record 'PR1' (riferimenti, dati destinatario, importi e particolarità consegna)
050100041210     C********
050200051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR1'
050300041210     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del FIVAB
050400041210     C                   IF        FlgNewBol = '1'
050500041210     C                   WRITE     FIVAB000
050600041210     C                   EVAL      FlgNewBol = '0'
050700041210     C                   ENDIF
050800041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
050900040910     C                   EXSR      INZVAR
051000040910     C                   EXSR      DEFCAM
051100041210     C                   EVAL      FlgNewBol = '1'
051200040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
051300040928     C                   MOVEL     datcor        VABAAS
051400040928     C                   MOVEL     datcor        VATAAS
051500040928     C                   MOVE      datcor        VABMGS
051600051228     C                   MOVE(P)   VABLNP        VABFGS
051700051228     C                   MOVE(P)   VABLNP        VATFGS
051800051206     C* ......VABNSP/VATNSP/VABRMN/VABRMA
051900051206     C                   EVAL      VABRMA=%trim(%subst(vindta:4:27))
052000051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:10))
052100051206     C                   EXSR      CHKNUM
052200051206     C                   IF        PiInt=*on
052300051206     C                   Z-ADD     PiVal         VABRMN
052400051206     C                   ELSE
052500051206     C                   ADD       1             errore
052600051206     C                   EVAL      vinmsg = %trimr(vinmsg)
052700051206     C                             + ' ' + 'VABRMN'
052800051206     C                   ENDIF
052900051206     C*
053000051206     C                   EVAL      PiStr=%trim(%subst(vindta:10:7))
053100051206     C                   EXSR      CHKNUM
053200051206     C                   IF        PiInt=*on
053300051206     C                   Z-ADD     PiVal         VABNSP
053400051206     C                   Z-ADD     PiVal         VATNSP
053500041210     C                   ELSE
053600041210     C                   ADD       1             errore
053700041210     C                   EVAL      vinmsg = %trimr(vinmsg)
053800051206     C                             + ' ' + 'VABNSP VATNSP'
053900041210     C                   ENDIF
054000051206     C* ......VABRMO
054100051206     C                   IF        %trim(%subst(vindta:46:35)) <>
054200051206     C                             'GEODIS ZUST AMBROSETTI'
054300051206     C                   EVAL      VABRMO=%trim(%subst(vindta:46:35))
054400051206     C                   ENDIF
054500051206     C* ......VATNOT + VATTRC = 'B'
054600051206     C                   IF        %trim(%subst(vindta:272:35)) <> *blanks
054700051206     C                   EVAL      VATNOT=%trim(%subst(vindta:272:35))
054800051206     C                   EVAL      VATTRC = 'B'
054900051206     C                   WRITE     FIVAT000
055000051206     C                   ENDIF
055100051206     C* ......VABRSD
055200051206     C                   EVAL      VABRSD=%trim(%subst(vindta:428:35))
055300050805     C* ......VABIND
055400051206     C                   EVAL      VABIND=%trim(%subst(vindta:498:35))
055500050805     C* ......VABLOD
055600051206     C                   EVAL      VABLOD=%trim(%subst(vindta:578:35))
055700050805     C* ......VABCAD
055800051206     C                   EVAL      PiStr=%trim(%subst(vindta:568:10))
055900050805     C                   EXSR      CHKNUM
056000050805     C                   IF        PiInt=*on
056100050805     C                   Z-ADD     PiVal         Num5_0
056200050805     C                   MOVEL(P)  Num5_0        VABCAD
056300050805     C                   ELSE
056400050805     C                   ADD       1             errore
056500050805     C                   EVAL      vinmsg = %trimr(vinmsg)
056600050805     C                             + ' ' + 'VABCAD'
056700050805     C                   ENDIF
056800051206     C* ......VABPRD
056900051206     C                   EVAL      VABPRD=%trim(%subst(vindta:613:3))
057000051206     C* ......VABCBO
057100051206     C                   IF        %trim(%subst(vindta:619:3)) = 'DDU'
057200051206     C                   EVAL      VABCBO='1'
057300051206     C                   ENDIF
057400051206     C                   IF        %trim(%subst(vindta:619:3)) = 'EXW'
057500051206     C                   EVAL      VABCBO='2'
057600051206     C                   ENDIF
057700051206     C* ......VABCAS/VABVCA
057800051206     C                   IF        %trim(%subst(vindta:628:15)) <> *zeros
057900051206     C                   EVAL      FlgCAS = '1'
058000051206     C*
058100051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBK'
058200051206     C                   EVAL      VABTIC='OM'
058300051206     C                   ENDIF
058400051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YBC'
058500051206     C                   EVAL      VABTIC='OM'
058600051206     C                   ENDIF
058700051206     C                   IF        %trim(%subst(vindta:643:3)) = 'YCB'
058800051206     C                   EVAL      VABTIC='OC'
058900051206     C                   ENDIF
059000051206     C                   IF        %trim(%subst(vindta:643:3)) = 'CSH'
059100051206     C                   EVAL      VABTIC=*blanks
059200051206     C                   ENDIF
059300051206     C*
059400051206     C                   EVAL      VABVCA=%trim(%subst(vindta:625:3))
059500051206     C*
059600051206     C                   EVAL      PiStr=%trim(%subst(vindta:628:15))
059700051206     C                   EXSR      CHKNUM
059800051206     C                   IF        PiNum=*on
059900051206     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
060000051206     C                   Z-ADD     PiVal         VABCAS
060100051206     C                   ELSE
060200051206     C                   ADD       1             errore
060300051206     C                   EVAL      vinmsg = %trimr(vinmsg)
060400051206     C                             + ' ' + 'VABCAS'
060500051206     C                   ENDIF
060600051206     C                   ENDIF
060700060307     C*
060800060307     C* Considerazioni sul contenuto di campi precedentemente valorizzati
060900060307     C                   IF        FlgCAS <> '0'
061000060307     C                   IF        VABCBO = '1'
061100060307     C                   EVAL      VABCBO = '4'
061200060307     C                   ELSE
061300060307     C                   EVAL      VABCBO = '6'
061400060307     C                   ENDIF
061500060307     C                   ENDIF
061600051206     C*
061700051206     C                   ENDIF
061800051206     C********
061900051206     C* FINE Tipo record 'PR1'
062000051206     C********
062100051206     C*
062200051206     C********
062300051206     C* Tipo record 'PR2' (note, consegna richiesta, valore da assicurare)
062400051206     C********
062500051206     C                   IF        %trim(%subst(vindta:1:3)) = 'PR2'
062600051206     C* ......VABDCR
062700051213     C                   IF        %subst(vindta:7:8)<>*blanks
062800051206     C                   EVAL      PiStr=%trim(%subst(vindta:7:8))
062900050805     C                   EXSR      CHKNUM
063000051206     C                   IF        PiInt=*on
063100051206     C                   Z-ADD     PiVal         VABDCR
063200050805     C                   ELSE
063300050805     C                   ADD       1             errore
063400050805     C                   EVAL      vinmsg = %trimr(vinmsg)
063500051206     C                             + ' ' + 'VABDCR'
063600050805     C                   ENDIF
063700051213     C                   ENDIF
063800051206     C* ......VABHCR
063900051213     C                   IF        %subst(vindta:7+8:4)<>*blanks
064000051206     C                   EVAL      PiStr=%trim(%subst(vindta:7+8:4))
064100051206     C                   EXSR      CHKNUM
064200051206     C                   IF        PiInt=*on
064300051206     C                   Z-ADD     PiVal         VABHCR
064400051206     C                   ELSE
064500051206     C                   ADD       1             errore
064600051206     C                   EVAL      vinmsg = %trimr(vinmsg)
064700051206     C                             + ' ' + 'VABHCR'
064800051206     C                   ENDIF
064900051213     C                   ENDIF
065000051206     C* ......VABNOT
065100051206     C                   EVAL      VABNOT=%trim(%subst(vindta:22:35))
065200051206     C* ......VABNT2
065300051206     C                   EVAL      VABNT2=%trim(%subst(vindta:22+35:35))
065400051206     C* ......VABIAS/VABVAS
065500051206     C                   IF        %subst(vindta:845:1) = 'S'
065600051206     C                   EVAL      VABVAS=%trim(%subst(vindta:846:3))
065700051206     C                   EVAL      PiStr=%trim(%subst(vindta:849:15))
065800051206     C                   EXSR      CHKNUM
065900051206     C                   IF        PiNum=*on
066000051206     C                   EVAL      PiVal = PiVal / 100
066100051206     C                   Z-ADD     PiVal         VABIAS
066200051206     C                   ELSE
066300051206     C                   ADD       1             errore
066400051206     C                   EVAL      vinmsg = %trimr(vinmsg)
066500051206     C                             + ' ' + 'VABIAS'
066600051206     C                   ENDIF
066700051206     C                   ENDIF
066800051206     C*
066900051206     C                   ENDIF
067000051206     C********
067100051206     C* FINE Tipo record 'PR2'
067200051206     C********
067300051206     C*
067400051206     C********
067500051206     C* Tipo record 'MER' (colli, peso)
067600051206     C********
067700051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MER'
067800050805     C* ......VABNCL
067900051206     C                   EVAL      PiStr=%trim(%subst(vindta:4:8))
068000050805     C                   EXSR      CHKNUM
068100050805     C                   IF        PiInt=*on
068200080208     C***                Z-ADD     PiVal         VABNCL
068300080208     C                   ADD       PiVal         VABNCL
068400050805     C                   ELSE
068500050805     C                   ADD       1             errore
068600050805     C                   EVAL      vinmsg = %trimr(vinmsg)
068700050805     C                             + ' ' + 'VABNCL'
068800050805     C                   ENDIF
068900050805     C* ......VABPKB
069000051206     C                   EVAL      PiStr=%trim(%subst(vindta:15:10))
069100050805     C                   EXSR      CHKNUM
069200050805     C                   IF        PiNum=*on
069300051206     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a kg
069400080208     C***                Z-ADD     PiVal         VABPKB
069500080208     C                   ADD       PiVal         VABPKB
069600050805     C                   ELSE
069700050805     C                   ADD       1             errore
069800050805     C                   EVAL      vinmsg = %trimr(vinmsg)
069900050805     C                             + ' ' + 'VABPKB'
070000050805     C                   ENDIF
070100061012     C* ......VABVLB
070200061012     C                   EVAL      PiStr=%trim(%subst(vindta:35:10))
070300061012     C                   EXSR      CHKNUM
070400061012     C                   IF        PiNum=*on
070500061012     C                   EVAL      PiVal = PiVal / 1000                         * da decim3 a metri3
070600080208     C***                Z-ADD     PiVal         VABVLB
070700080208     C                   ADD       PiVal         VABVLB
070800061012     C                   ELSE
070900061012     C                   ADD       1             errore
071000061012     C                   EVAL      vinmsg = %trimr(vinmsg)
071100061012     C                             + ' ' + 'VABVLB'
071200061012     C                   ENDIF
071300050805     C*
071400050805     C                   ENDIF
071500051206     C********
071600051206     C* FINE Tipo record 'MER'
071700051206     C********
071800051206     C*
071900051206     C********
072000051206     C* Tipo record 'MAR' (barcode)
072100051206     C********
072200051206     C                   IF        %trim(%subst(vindta:1:3)) = 'MAR'
072300051206     C* ......VATNOT + VATTRC = 'E'
072400051213     C                   IF        %trim(%subst(vindta:7:20)) <> *blanks
072500051213     C                   EVAL      VATNOT=%trim(%subst(vindta:7:20))
072600051206     C                   EVAL      VATTRC = 'E'
072700051206     C                   WRITE     FIVAT000
072800051206     C                   ENDIF
072900050805     C*
073000050805     C                   ENDIF
073100051206     C********
073200051206     C* FINE Tipo record 'MAR'
073300051206     C********
073400040910     C*
073500040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
073600040910     C                   EXSR      CHKIMPDIV
073700010202     C*
073800000801     C* Ebbene...
073900000801     C                   ADD       1             §CTRMO
074000010201     C                   IF        errore <> *zeros
074100000801     C                   ADD       1             §CTRNO
074200000801     C                   EVAL      vinflg = '2'
074300000801     C                   ELSE
074400010201     C                   ADD       1             §CTROKVB
074500000801     C                   ENDIF
074600000801     C*
074700000801     C                   ENDSR
074800010202     C*----------------------------------------------------*
074900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
075000010202     C*----------------------------------------------------*
075100020305     C     PREVAT        BEGSR
075200010202     C*
075300021113     C* Compongo il nome del membro da dare al FIVATWWR
075400010202     C                   eval      parmbr = vlrhdl
075500010202     C                   movel     'M'           parmbr
075600041210     C                   eval      parccm = vlrksc
075700010202     C                   eval      paropz = '1'
075800010202     C* Effettuo la chiamata al CLLE preposto
075900040506     C                   call(e)   'TITVVTC'
076000010202     C                   parm                    parccm
076100010202     C                   parm                    parmbr
076200010202     C                   parm                    paropz
076300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076400010202     C                   if        %error
076500010202     C                   movel     '1'           chkcall
076600010202     C                   else
076700010202     C                   movel     '0'           chkcall
076800010202     C                   endif
076900010202     C*
077000010202     C                   ENDSR
077100000801     C*----------------------------------------------------*
077200000801     C*  CONTROLLO NUMERICITA' CAMPI
077300000801     C*----------------------------------------------------*
077400000801     C     CHKNUM        BEGSR
077500000801     C*
077600000801     C                   call(e)   'ISNUMERIC'
077700000801     C                   PARM                    PiStr            30
077800041210     C                   PARM      '.'           PiDecChr          1
077900000801     C                   PARM      *ZEROS        PiVal            30 9
078000000801     C                   PARM      '0'           PiInt             1
078100000801     C                   PARM      '0'           PiNum             1
078200000801     C                   IF        %error
078300000801     C                   EVAL      PiInt=*off
078400000801     C                   ENDIF
078500000801     C*
078600000801     C                   ENDSR
078700000801     C***
078800000801
078900011113
079000011113     C*----------------------------------------------------*
079100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
079200011113     C*----------------------------------------------------*
079300011113     C     CHKIMPDIV     BEGSR
079400011113     C*
079500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
079600011113     C                   Z-ADD     *zeros        wrkDec            9 9
079700011113     C*
079800011113     C* Come prima cosa effettuo considerazioni sulla divisa
079900011113     C                   IF        vabIAS > *zeros
080000011113     C                   IF        vabVAS <> 'EUR'
080100011113     C                   EVAL      vabVAS =  'ITL'
080200011113     C                   ENDIF
080300011113     C                   ENDIF
080400011113     C*
080500011113     C                   IF        vabCAS > *zeros
080600011113     C                   IF        vabVCA <> 'EUR'
080700011113     C                   EVAL      vabVCA =  'ITL'
080800011113     C                   ENDIF
080900011113     C                   ENDIF
081000011113     C*
081100011113     C                   IF        vabVMD > *zeros
081200020305     C                   IF        vabVAD <> 'EUR'
081300011113     C                   EVAL      vabVAD =  'ITL'
081400011113     C                   ENDIF
081500011113     C                   ENDIF
081600011113     C*
081700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
081800011113     C                   Z-ADD     vabIAS        wrkDec
081900011113     C                   IF        wrkDec > *zeros
082000011113     C                   IF        vabVAS = 'ITL'
082100011113     C                   EVAL      vabIAS = *zeros
082200011113     C                   ENDIF
082300011113     C                   ENDIF
082400011113     C*
082500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
082600011113     C                   Z-ADD     vabCAS        wrkDec
082700011113     C                   IF        wrkDec > *zeros
082800011113     C                   IF        vabVCA = 'ITL'
082900011113     C                   EVAL      vabCAS = *zeros
083000011113     C                   ENDIF
083100011113     C                   ENDIF
083200011113     C*
083300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
083400011113     C                   Z-ADD     vabVMD        wrkDec
083500011113     C                   IF        wrkDec > *zeros
083600011113     C                   IF        vabVAD = 'ITL'
083700011113     C                   EVAL      vabVMD = *zeros
083800011113     C                   ENDIF
083900011113     C                   ENDIF
084000011113     C*
084100011113     C                   ENDSR
084200011113     C***
084300011113
084400011113
084500000801
084600000801
084700990920      /TITLE Invio dei dati al punto operativo.
084800010202     C     invio         BEGSR
084900990920     C*
085000021113     C* 1° invio FIVAT
085100010201     C                   reset                   dscmz
085200051228     C                   move      VATLNP        cmzdst
085300021113     C                   eval      cmzfld = 'FIVATWWR'
085400010201     C                   eval      cmzmbd = vlrhdl
085500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085600021009     C***                if        prmfir = *blanks
085700021113     C                   eval      cmzfla = 'FIVAT00F'
085800021113     C                   eval      cmzmba = 'FIVAT00F'
085900021009     C***                else
086000021009     C***                eval      cmzfla = prmfir
086100021009     C***                eval      cmzmba = prmfir
086200021009     C***                endif
086300010201     C                   eval      cmznrr = *zeros
086400020305     C                   move      §ctrokvt      cmznrr
086500021018     C                   eval      cmzlba = vlrfl1
086600010201     C                   call(e)   'TIS711C'
086700010201     C                   parm                    dscmz
086800010201     C                   parm      *blanks       esito
086900010205     C                   if        %error
087000010205     C                             or cmzerr = '1'
087100010205     C                             or esito  = '1'
087200010205     C                   eval      wrkesito = '3'
087300010205     C                   else
087400010201     C*
087500021113     C* 2° invio FIVAB
087600010201     C                   reset                   dscmz
087700051228     C                   move      VABLNP        cmzdst
087800010201     C                   eval      cmzfld = vlrfou
087900010201     C                   eval      cmzmbd = vlrhdl
088000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
088100021009     C***                if        prmfir = *blanks
088200021113     C                   eval      cmzfla = 'FIVAB00F'
088300021113     C                   eval      cmzmba = 'FIVAB00F'
088400021009     C***                else
088500021009     C***                eval      cmzfla = prmfir
088600021009     C***                eval      cmzmba = prmfir
088700021009     C***                endif
088800010201     C                   eval      cmznrr = *zeros
088900010201     C                   move      §ctrokvb      cmznrr
089000021018     C                   eval      cmzlba = vlrfl1
089100010201     C                   call(e)   'TIS711C'
089200010201     C                   parm                    dscmz
089300010201     C                   parm      *blanks       esito
089400010201     C                   if        %error
089500010201     C                             or cmzerr = '1'
089600010201     C                             or esito  = '1'
089700010201     C                   eval      wrkesito = '3'
089800010201     C                   endif
089900010205     C                   endif
090000990920     C*
090100000613     C                   ENDSR
090200000613     C***
090300070411
090400070411     C     *pssr         BEGSR
090500070411     C*
090600070411     C                   if        %open(tivin00r)
090700070411     C                   close     tivin00r
090800070411     C                   endif
090900070411     C                   if        %open(fivabwwr)
091000070411     C                   close     fivabwwr
091100070411     C                   endif
091200070411     C                   if        %open(fivatwwr)
091300070411     C                   close     fivatwwr
091400070411     C                   endif
091500070411     C*
091600070411     C* Effettuo la chiamata al CLLE preposto
091700070411     C                   call(e)   'TITVVTC'
091800070411     C                   parm                    parccm
091900070411     C                   parm                    parmbr
092000070411     C                   parm      '2'           paropz
092100070411     C*
092200070411     C                   eval      wrkesito = '2'
092300070411     C*
092400070411     C                   seton                                        LR
092500070411     C*
092600070411     C                   ENDSR     '*CANCL'
092700070411     C***
092800070411
092900990910
093000000613     C     *inzsr        BEGSR
093100990910     C*
093200990910     C     *entry        plist
093300990920     C                   parm                    tivlrds
093400990921     C                   parm      wrkesito      esito
093500000724     C                   parm                    prmlit
093600000710     C                   parm                    prmfir
093700000613     C*
093800000830     C* CALCOLA LA DATA CORRENTE
093900121030     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
094000121030     C                   eval      datcor = %dec(%date() : *ISO)
094100000830     C*
094200000613     C                   ENDSR
094300000613     C***
