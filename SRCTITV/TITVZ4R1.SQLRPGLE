000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100070730     D depspe          s             10    INZ(*blanks)
002200010213     D depdat          s              8  0 INZ(*zeros)
002300010213     D depaas          s              4    INZ(*zeros)
002400070730     D curspe          s             10    INZ(*blanks)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000070530     D Num5_0          s              5  0
003100000830
003200000830     D*------------------
003300000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003400000830     D*------------------
003500000830     D WLBDA8          DS                  INZ
003600000830     D  G08DAT                 1      8  0
003700000830     D  G08INV                 9     16  0
003800000830     D  G08ERR                17     17
003900000830     D  G08TGI                18     22  0
004000000830     D*
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600020725     D  VINDTA                 1   2048
004700020725     D  VINFLG              2049   2049
004800020725     D  VINSPE              2050   2059
004900020725     D  VINRRN              2060   2067  0
005000020725     D*
005100990908
005200010201
005300071120     C*
005400071120     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005500071120     C
005600071120     C/EXEC SQL
005700071120     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005800071120     C/END-EXEC
005900071120     C*
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000613     C*
006400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700050128     C                   call(e)   'TITVVTC'
006800010202     C                   parm                    parccm
006900010202     C                   parm                    parmbr
007000010202     C                   parm      '2'           paropz
007100070730     C*
007200070730     C* Effettuo lancio TISI95 solo x chiusura
007300070730     C                   CLEAR                   TISI95DS
007400070730     C                   EVAL      I95TLA = 'C'
007500070730     C                   CALL      'TISI95R'
007600070730     C                   PARM                    TISI95DS
007700000616     C*
007800000801     C
007900010201     C                   seton                                        LR
008000990908
008100000801
008200910830     C*--------------------------------------------------------
008300070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
008400910830     C*--------------------------------------------------------
008500070530     C     RWFILE        BEGSR
008600990910     C*
008700990914     C                   if        not %open(tivin00r)
008800990908     C                   open      tivin00r
008900990914     C                   endif
009000021113     C                   if        not %open(fivabwwr)
009100021113     C                   open      fivabwwr
009200990914     C                   endif
009300070530     C*
009400021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
009500020305     C                   exsr      prevat
009600010201     C*
009700010202     C                   if        chkcall = '0'
009800010202     C*
009900021113     C                   if        not %open(fivatwwr)
010000021113     C                   open      fivatwwr
010100010201     C                   endif
010200990910     C*
010300010201     C                   clear                   §CTROKVB          5 0
010400020305     C                   clear                   §CTROKVT          5 0
010500000801     C                   clear                   §CTRMO            5 0
010600000801     C                   clear                   §CTRNO            5 0
010700990910     C*
010800020725     C/EXEC SQL
010900020725     C+ declare C1 cursor for select
011000070730     C+ vindta, vinflg, substr(vindta, 3, 10) as sped, rrn(tivin00r)
011100060223     C+ from tivin00r
011200060223     C+ order by sped
011300020725     C+ for read only
011400020725     C/END-EXEC
011500020725     C
011600020725     C/EXEC SQL
011700020725     C+ open C1
011800020725     C/END-EXEC
011900020725     C
012000020725     C/EXEC SQL
012100070530     C+ Fetch C1 into :INPUT_DS
012200020725     C/END-EXEC
012300020725     C*
012400020725     C                   dow       sqlcod = *zeros
012500990913     C*
012600020725     C                   if        vindta > *blanks
012700000613     C                   add       1             rrnum
012800000801     C*
012900020725     C                   if        vinflg = *blanks
013000020725     C                             or vinflg = '0'
013100020725     C                             or vinflg = '2'
013200000801     C*
013300020725     C                   clear                   x_vinmsg
013400020725     C                   eval      x_vinflg = '1'
013500010305     C*
013600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
013700070730     C                   EVAL      PiStr=%trim(%subst(vindta:3:10))
013800020305     C                   MOVEL(p)  PiStr         curspe
013900010305     C*
014000010305     C                   if        depspe = *blanks                             => 1° giro
014100010305     C                   eval      depspe = curspe                              => memorizz. spediz
014200020725     C                   clear                   tivinds
014300020725     C                   exsr      inzvar
014400020725     C                   exsr      defcam
014500020305     C                   exsr      impvab
014600100506     C                   exsr      wrivat_e                                     => carico VAT
014700010305     C                   else
014800020725     C                   if        depspe <> curspe                             => rottura di spediz
014900010305     C                   eval      depspe = curspe                              => memorizz. spediz
015000020305     C                   exsr      wrivab
015100020725     C                   clear                   tivinds
015200020725     C                   exsr      inzvar
015300020725     C                   exsr      defcam
015400020305     C                   exsr      impvab
015500100506     C                   exsr      wrivat_e                                     => carico VAT
015600020305     C                   else                                                   => x stessa spediz
015700020305     C                   exsr      impvab
015800100506     C                   exsr      wrivat_e                                     => carico VAT
015900010305     C                   endif
016000010305     C                   endif
016100010305     C                   endif
016200000905     C*
016300000905     C                   else
016400020725     C                   eval      x_vinflg = '1'
016500000905     C                   endif
016600000905     C*
016700020725     C     VINRRN        chain     tivin000
016800020725     C                   if        %found(tivin00r)
016900020725     C                   eval      y_vinflg = x_vinflg
017000020725     C                   eval      y_vinmsg = x_vinmsg
017100020725     C                   update    tivin000
017200020725     C                   endif
017300020725     C*
017400020725     C/EXEC SQL
017500070530     C+ Fetch C1 into :INPUT_DS
017600020725     C/END-EXEC
017700020725     C*
017800020725     C                   enddo
017900020725     C*
018000020725     C/EXEC SQL
018100020725     C+ close C1
018200020725     C/END-EXEC
018300000905     C*
018400020305     C* Scarico i VAB rimasti "in sospeso"
018500020305     C                   exsr      wrivab
018600010202     C*
018700010202     C                   endif
018800990910
018900990910     C* Se non ci sono record con errori ...
019000000710     C                   if        §ctrno = 0
019100990910     C* ... restituisco esito OK.
019200990921     C                   eval      wrkesito = '0'
019300990910     C                   else
019400010201     C                   if        §ctrokvb > 0
019500990921     C                   eval      wrkesito = '1'
019600000710     C                   else
019700000710     C                   eval      wrkesito = '2'
019800990910     C                   endif
019900000710     C                   endif
020000990910     C*
020100990914     C                   if        %open(tivin00r)
020200990908     C                   close     tivin00r
020300990914     C                   endif
020400021113     C                   if        %open(fivabwwr)
020500021113     C                   close     fivabwwr
020600990914     C                   endif
020700021113     C                   if        %open(fivatwwr)
020800021113     C                   close     fivatwwr
020900010201     C                   endif
021000990910     C*
021100010201     C                   if        §ctrokvb > 0
021200000724     C                             and vlrpoi <> *zeros
021300010202     C                   exsr      invio
021400990920     C                   endif
021500990920     C*
021600910830     C                   ENDSR
021700000613     C***
021800010305
021900010305     C*----------------------------------------------------*
022000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022100010305     C*----------------------------------------------------*
022200020305     C     WRIVAB        BEGSR
022300100426     C*
022400100505     C                   exsr      CALFGS
022500100505     C*
022600021113     C                   write     fivab000                                     => scarico il VAB
022700070530     C*
022800070530     C* Insieme alla testata scarico le estensini destinatario su VAT - tipi record A e B
022900070530     C                   if        wVAT_A <> *blanks
023000070530     C                   eval      VATTRC = 'A'
023100070530     C                   eval      VATNOT = wVAT_A
023200100505     C                   exsr      WRIVAT
023300070530     C                   endif
023400070530     C*
023500070530     C                   if        wVAT_B <> *blanks
023600070530     C                   eval      VATTRC = 'B'
023700070530     C                   eval      VATNOT = wVAT_B
023800100505     C                   exsr      WRIVAT
023900070530     C                   endif
024000010305     C*
024100010305     C                   ENDSR
024200100505
024300100505     C*----------------------------------------------------*
024400100505     C*  CALCOLO P.O. GESTIONE
024500100505     C*----------------------------------------------------*
024600100505     C     CALFGS        BEGSR
024700100505     C*
024800100505     C                   z-add     *zeros        invfil            3 0
024900100505     C*
025000100505     C                   if        vlrpoi = 999
025100100505     C                   move(P)   vablnp        VABFGS
025200100505     C                   move(P)   vablnp        VATFGS
025300100505     C                   move(P)   vablnp        invfil
025400100505     C                   else
025500100505     C                   move(P)   vlrpoi        VABFGS
025600100505     C                   move(P)   vlrpoi        VATFGS
025700100505     C                   move(P)   vlrpoi        invfil
025800100505     C                   endif
025900100505     C*
026000100505     C                   ENDSR
026100990920
026200000801     C*----------------------------------------------------*
026300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400000801     C*----------------------------------------------------*
026500010201     C     INZVAR        BEGSR
026600000801     C*
026700010201     C                   Z-ADD     *zeros        Num5_0
026800070530     C                   MOVEL     *blanks       wVAT_A           30
026900070530     C                   MOVEL     *blanks       wVAT_B           30
027000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027100020725     C                   MOVEL     '0'           FlgCAS            1
027200000801     C*
027300000801     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700020904     C     DEFCAM        BEGSR
027800000801     C*
027900021113     C                   CLEAR                   FIVAB000
028000021113     C                   CLEAR                   FIVAT000
028100070730     C* Imposto i valori di default...
028200080702     C                   EVAL      VABCCM = 2022449
028300080702     C                   EVAL      VATCCM = 2022449
028400080702     C                   EVAL      VABLNP = 202
028500080702     C                   EVAL      VATLNP = 202
028600070730     C                   EVAL      VABCTR = 100
028700070730     C                   EVAL      VABCBO = '1'
028800080702     C                   MOVEL(P)  '7'           VABCTM
028900070530     C                   MOVEL     '1'           VABCBO
029000070222     C* ... e poi verifico se sono stati passati come parametri
029100070222     C                   IF        vlrppt > *blanks
029200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029400070222     C                   EXSR      CHKNUM
029500070222     C                   IF        PiInt=*on
029600070222     C                   Z-ADD     PiVal         VABCCM
029700070222     C                   Z-ADD     PiVal         VATCCM
029800070222     C                   ENDIF
029900070222     C                   ENDIF
030000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030200070222     C                   EXSR      CHKNUM
030300070222     C                   IF        PiInt=*on
030400070222     C                   Z-ADD     PiVal         VABLNP
030500070222     C                   Z-ADD     PiVal         VATLNP
030600070222     C                   ENDIF
030700070222     C                   ENDIF
030800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
030900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031000070222     C                   EXSR      CHKNUM
031100070222     C                   IF        PiInt=*on
031200070222     C                   Z-ADD     PiVal         VABCTR
031300070222     C                   ENDIF
031400070222     C                   ENDIF
031500070222     C                   ENDIF
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032000000801     C*----------------------------------------------------*
032100010201     C     IMPVAB        BEGSR
032200070530     C*
032300070530     C                   SETOFF                                       3132
032400020725     C*
032500070530     C* Campi anno e mese/giorno
032600070530     C                   MOVEL     datcor        VABAAS
032700070530     C                   MOVEL     datcor        VATAAS
032800070530     C                   MOVE      datcor        VABMGS
032900070730     C*
033000070730     C* Reperimento campi ALFA
033100070730     C*
033200070730     C* Considerazioni sulla ragione sociale del destinatario da indicare
033300070730     C                   EVAL      VABRSD=%trim(%subst(vindta:88:30))
033400070730     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033500070730     C     '@':'A'       XLATE     VABRSD        VABRSD
033600070730     C* ==
033700070730     C                   EVAL      VABIND=%trim(%subst(vindta:118:30))
033800070730     C                   EVAL      VABLOD=%trim(%subst(vindta:148:30))
033900070730     C                   EVAL      VABPRD=%trim(%subst(vindta:183:2))
034000071120     C                   MOVEL     *blanks       wNOTE            40
034100071120     C                   EVAL      wNOTE=%trim(%subst(vindta:253:40))
034200071120     C                   EVAL      VABNOT=%trim(%subst(wNOTE:1:35))
034300071120     C                   EVAL      VABNT2=%trim(%subst(wNOTE:1+35:5))
034400101018     C                   EVAL      VABRMA=%trim(%subst(vindta:3:1))+
034500101018     C                                    %trim(%subst(vindta:48:11))
034600090122     C*
034700090122     C* Forzature cliente / tariffa
034800100422     C                   SELECT
034900100422     C                   WHEN      %subst(vindta:3:1) = 'G'
035000090122     C                   EVAL      VABCCM = 2022449
035100090122     C                   EVAL      VATCCM = 2022449
035200100422     C                   EVAL      VABLNP = 202
035300100422     C                   EVAL      VATLNP = 202
035400090122     C                   EVAL      VABCTR = 100
035500100422     C                   WHEN      %subst(vindta:3:1) = 'H'
035600090122     C                   EVAL      VABCCM = 2022515
035700090122     C                   EVAL      VATCCM = 2022515
035800100422     C                   EVAL      VABLNP = 202
035900100422     C                   EVAL      VATLNP = 202
036000090122     C                   EVAL      VABCTR = 110
036100100422     C                   WHEN      %subst(vindta:3:1) = 'F'
036200100422     C                   EVAL      VABCCM = 0063658
036300100422     C                   EVAL      VATCCM = 0063658
036400120116     C                   EVAL      VABLNP = 214
036500120116     C                   EVAL      VATLNP = 214
036600100422     C                   EVAL      VABCTR = 100
036700100505     C                   EVAL      VABCTM = '7Q'
036800100422     C                   ENDSL
036900070730     C*
037000070730     C* Reperimento campi NUMERICI
037100070730     C* CCM
037200100422     C                   IF        %subst(vindta:3:1) = 'C'
037300070730     C                   ENDIF
037400070730     C                   IF        %subst(vindta:3:1) = 'D'
037500070730     C                   ENDIF
037600070730     C* NSP/RMN
037700101018     C                   SELECT
037800101020     C                   WHEN      %subst(vindta:3:1) = 'F'
037900101020     C                   EVAL      PiStr='66'+%trim(%subst(vindta:4:7))
038000101020     C                   WHEN      %subst(vindta:3:1) = 'G'
038100101020     C                   EVAL      PiStr='77'+%trim(%subst(vindta:4:7))
038200101018     C                   OTHER
038300101018     C                   EVAL      PiStr='88'+%trim(%subst(vindta:4:7))
038400101018     C                   ENDSL
038500070730     C                   EXSR      CHKNUM
038600070730     C                   IF        PiInt=*on
038700070730     C                   Z-ADD     PiVal         VABNSP
038800070730     C                   Z-ADD     PiVal         VATNSP
038900070730     C                   Z-ADD     PiVal         VABRMN
039000070730     C                   ELSE
039100070730     C                   SETON                                        31
039200070730     C                   Z-ADD     *zeros        VABNSP
039300070730     C                   Z-ADD     *zeros        VATNSP
039400070730     C                   Z-ADD     1             VABRMN
039500070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039600070730     C                             + ' ' + 'VABNSP VATNSP VABRMN'
039700070730     C                   ENDIF
039800070730     C* CAD
039900070730     C                   EVAL      PiStr=%trim(%subst(vindta:178:5))
040000070730     C                   EXSR      CHKNUM
040100070730     C                   IF        PiInt=*on
040200070730     C                   Z-ADD     PiVal         Num5_0
040300070730     C                   MOVEL(p)  Num5_0        VABCAD
040400070730     C                   ELSE
040500070730     C                   SETON                                        32
040600070730     C                   EVAL      VABCAD = *zeros
040700070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800070730     C                             + ' ' + 'VABCAD'
040900070730     C                   ENDIF
041000070730     C* Reperisco la provincia dal CAP e dalla località
041100070730     C                   IF        VABCAD <> *blanks AND
041200070730     C                             VABPRD  = *blanks
041300070730     C                   CLEAR                   TISI95DS
041400070730     C                   EVAL      I95TCN = '3'
041500070730     C                   Z-ADD     datcor        I95DAT
041600070730     C                   EVAL      I95CAP = VABCAD
041700070730     C                   EVAL      I95LOC = VABLOD
041800070730     C                   EVAL      I95NAR = VABNZD
041900070730     C                   CALL      'TISI95R'
042000070730     C                   PARM                    TISI95DS
042100070730     C                   EVAL      VABPRD = O95PRV
042200070730     C                   ENDIF
042300070730     C* NCL
042400070730     C                   EVAL      PiStr=%trim(%subst(vindta:192:5))
042500070730     C                   EXSR      CHKNUM
042600070730     C                   IF        PiInt=*on
042700070730     C                   Z-ADD     PiVal         VABNCL
042800070730     C                   ELSE
042900070730     C                   SETON                                        32
043000070730     C                   Z-ADD     *zeros        VABNCL
043100070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043200070730     C                             + ' ' + 'VABNCL'
043300070730     C                   ENDIF
043400070730     C* PKB
043500070730     C                   EVAL      PiStr=%trim(%subst(vindta:185:7))
043600070730     C                   EVAL      PiDecChr = '.'
043700070730     C                   EXSR      CHKNUM
043800070730     C                   IF        PiNum=*on
043900070730     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
044000070730     C                   Z-ADD     PiVal         VABPKB
044100070730     C                   ELSE
044200070730     C                   SETON                                        32
044300070730     C                   Z-ADD     *zeros        VABPKB
044400070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044500070730     C                             + ' ' + 'VABPKB'
044600070730     C                   ENDIF
044700070730     C* CAS
044800070730     C                   IF        %trim(%subst(vindta:296:11)) <> *all'0'
044900070730     C                   EVAL      VABVCA = %trim(%subst(vindta:293:3))
045000070730     C                   EVAL      FlgCAS = '1'
045100070730     C                   EVAL      VABTIC = 'BM'
045200070730     C                   EVAL      PiStr=%trim(%subst(vindta:296:11))
045300070730     C                   EXSR      CHKNUM
045400070730     C                   IF        PiNum=*on
045500070730     C                   EVAL(H)   PiVal = PiVal / 100                          * gest. 2 decimali
045600070730     C                   Z-ADD     PiVal         VABCAS
045700070730     C                   ELSE
045800070730     C                   SETON                                        32
045900070730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046000070730     C                             + ' ' + 'VABCAS'
046100070730     C                   ENDIF
046200070730     C                   ENDIF
046300071204     C*
046400071204     C* Verifico se richiesta consegna x appuntamento
046500071204     C                   IF        %scan('PS#': wNOTE) > *zeros OR
046600071204     C                             %scan('ps#': wNOTE) > *zeros
046700071204     C                   EVAL      VABTC1 = 'A'
046800071204     C                   ENDIF
046900071219     C*
047000071219     C* Verifico se richiesto incasso contrassegno TM
047100071219     C                   IF        %scan('TM#': wNOTE) > *zeros OR
047200071219     C                             %scan('tm#': wNOTE) > *zeros
047300071219     C                   EVAL      VABTIC = 'BM'
047400071219     C                   ENDIF
047500070730     C*
047600070730     C* Considerazioni finali su CBO/CAS
047700070730     C                   IF        FlgCAS = '1'
047800070730     C                   IF        VABCBO = '1'
047900070730     C                   EVAL      VABCBO = '4'
048000070730     C                   ENDIF
048100070730     C                   IF        VABCBO = '2'
048200070730     C                   EVAL      VABCBO = '6'
048300070730     C                   ENDIF
048400070730     C                   ENDIF
048500020305     C*
048600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048700011113     C                   EXSR      CHKIMPDIV
048800010202     C*
048900000801     C* Ebbene...
049000000801     C                   ADD       1             §CTRMO
049100070530     C                   IF        *in31 <> *zeros OR
049200070530     C                             *in32 <> *zeros
049300000801     C                   ADD       1             §CTRNO
049400020725     C                   EVAL      x_vinflg = '2'
049500000801     C                   ELSE
049600010201     C                   ADD       1             §CTROKVB
049700000801     C                   ENDIF
049800000801     C*
049900000801     C                   ENDSR
050000100505     C*----------------------------------------------------*
050100100505     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
050200100505     C*----------------------------------------------------*
050300100505     C     WRIVAT_E      BEGSR
050400100505     C*
050500100505     C* Gestione particolare solo x traffico PADOVA
050600100505     C                   if        %subst(vindta:3:1) = 'F'
050700100505     C                   eval      VATTRC = 'E'
050800100505     C                   eval      VATNOT = %trim(%subst(vindta:357:20))
050900100505     C                   exsr      WRIVAT
051000100505     C                   endif
051100100505     C*
051200100505     C                   ENDSR
051300010201     C*----------------------------------------------------*
051400021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
051500010201     C*----------------------------------------------------*
051600020305     C     WRIVAT        BEGSR
051700100505     C*
051800100505     C                   exsr      CALFGS
051900100505     C*
052000100505     C                   if        VATNOT <> *blanks
052100100505     C                   write     FIVAT000
052200100505     C                   ADD       1             §CTROKVT
052300100505     C                   endif
052400010201     C*
052500010201     C                   ENDSR
052600020904
052700020904
052800010202     C*----------------------------------------------------*
052900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053000010202     C*----------------------------------------------------*
053100020305     C     PREVAT        BEGSR
053200010202     C*
053300021113     C* Compongo il nome del membro da dare al FIVATWWR
053400010202     C                   eval      parmbr = vlrhdl
053500010202     C                   movel     'M'           parmbr
053600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
053700010202     C                   eval      paropz = '1'
053800010202     C* Effettuo la chiamata al CLLE preposto
053900050128     C                   call(e)   'TITVVTC'
054000010202     C                   parm                    parccm
054100010202     C                   parm                    parmbr
054200010202     C                   parm                    paropz
054300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054400010202     C                   if        %error
054500010202     C                   movel     '1'           chkcall
054600010202     C                   else
054700010202     C                   movel     '0'           chkcall
054800010202     C                   endif
054900010202     C*
055000010202     C                   ENDSR
055100000801     C*----------------------------------------------------*
055200000801     C*  CONTROLLO NUMERICITA' CAMPI
055300000801     C*----------------------------------------------------*
055400000801     C     CHKNUM        BEGSR
055500000801     C*
055600000801     C                   call(e)   'ISNUMERIC'
055700000801     C                   PARM                    PiStr            30
055800070530     C                   PARM      '.'           PiDecChr          1
055900000801     C                   PARM      *ZEROS        PiVal            30 9
056000000801     C                   PARM      '0'           PiInt             1
056100000801     C                   PARM      '0'           PiNum             1
056200000801     C                   IF        %error
056300000801     C                   EVAL      PiInt=*off
056400000801     C                   ENDIF
056500000801     C*
056600000801     C                   ENDSR
056700000801     C***
056800000801
056900011113
057000011113     C*----------------------------------------------------*
057100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057200011113     C*----------------------------------------------------*
057300011113     C     CHKIMPDIV     BEGSR
057400011113     C*
057500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057600011113     C                   Z-ADD     *zeros        wrkDec            9 9
057700011113     C*
057800011113     C* Come prima cosa effettuo considerazioni sulla divisa
057900011113     C                   IF        vabIAS > *zeros
058000011113     C                   IF        vabVAS <> 'EUR'
058100011113     C                   EVAL      vabVAS =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C                   IF        vabCAS > *zeros
058600011113     C                   IF        vabVCA <> 'EUR'
058700011113     C                   EVAL      vabVCA =  'ITL'
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C                   IF        vabVMD > *zeros
059200020305     C                   IF        vabVAD <> 'EUR'
059300011113     C                   EVAL      vabVAD =  'ITL'
059400011113     C                   ENDIF
059500011113     C                   ENDIF
059600011113     C*
059700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
059800011113     C                   Z-ADD     vabIAS        wrkDec
059900011113     C                   IF        wrkDec > *zeros
060000011113     C                   IF        vabVAS = 'ITL'
060100011113     C                   EVAL      vabIAS = *zeros
060200011113     C                   ENDIF
060300011113     C                   ENDIF
060400011113     C*
060500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
060600011113     C                   Z-ADD     vabCAS        wrkDec
060700011113     C                   IF        wrkDec > *zeros
060800011113     C                   IF        vabVCA = 'ITL'
060900011113     C                   EVAL      vabCAS = *zeros
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061400011113     C                   Z-ADD     vabVMD        wrkDec
061500011113     C                   IF        wrkDec > *zeros
061600011113     C                   IF        vabVAD = 'ITL'
061700011113     C                   EVAL      vabVMD = *zeros
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C                   ENDSR
062200011113     C***
062300011113
062400011113
062500000801
062600000801
062700990920      /TITLE Invio dei dati al punto operativo.
062800010202     C     invio         BEGSR
062900990920     C*
063000021113     C* 1° invio FIVAT
063100010201     C                   reset                   dscmz
063200100426     C                   move      invfil        cmzdst
063300021113     C                   eval      cmzfld = 'FIVATWWR'
063400010201     C                   eval      cmzmbd = vlrhdl
063500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063600021009     C***                if        prmfir = *blanks
063700021113     C                   eval      cmzfla = 'FIVAT00F'
063800021113     C                   eval      cmzmba = 'FIVAT00F'
063900021009     C***                else
064000021009     C***                eval      cmzfla = prmfir
064100021009     C***                eval      cmzmba = prmfir
064200021009     C***                endif
064300010201     C                   eval      cmznrr = *zeros
064400020305     C                   move      §ctrokvt      cmznrr
064500021018     C                   eval      cmzlba = vlrfl1
064600010201     C                   call(e)   'TIS711C'
064700010201     C                   parm                    dscmz
064800010201     C                   parm      *blanks       esito
064900010205     C                   if        %error
065000010205     C                             or cmzerr = '1'
065100010205     C                             or esito  = '1'
065200010205     C                   eval      wrkesito = '3'
065300010205     C                   else
065400010201     C*
065500021113     C* 2° invio FIVAB
065600010201     C                   reset                   dscmz
065700100426     C                   move      invfil        cmzdst
065800010201     C                   eval      cmzfld = vlrfou
065900010201     C                   eval      cmzmbd = vlrhdl
066000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066100021009     C***                if        prmfir = *blanks
066200021113     C                   eval      cmzfla = 'FIVAB00F'
066300021113     C                   eval      cmzmba = 'FIVAB00F'
066400021009     C***                else
066500021009     C***                eval      cmzfla = prmfir
066600021009     C***                eval      cmzmba = prmfir
066700021009     C***                endif
066800010201     C                   eval      cmznrr = *zeros
066900010201     C                   move      §ctrokvb      cmznrr
067000021018     C                   eval      cmzlba = vlrfl1
067100010201     C                   call(e)   'TIS711C'
067200010201     C                   parm                    dscmz
067300010201     C                   parm      *blanks       esito
067400010201     C                   if        %error
067500010201     C                             or cmzerr = '1'
067600010201     C                             or esito  = '1'
067700010201     C                   eval      wrkesito = '3'
067800010201     C                   endif
067900010205     C                   endif
068000990920     C*
068100000613     C                   ENDSR
068200000613     C***
068300070411
068400070411     C     *pssr         BEGSR
068500070411     C*
068600070411     C                   if        %open(tivin00r)
068700070411     C                   close     tivin00r
068800070411     C                   endif
068900070411     C                   if        %open(fivabwwr)
069000070411     C                   close     fivabwwr
069100070411     C                   endif
069200070411     C                   if        %open(fivatwwr)
069300070411     C                   close     fivatwwr
069400070411     C                   endif
069500070411     C*
069600070411     C* Effettuo la chiamata al CLLE preposto
069700070411     C                   call(e)   'TITVVTC'
069800070411     C                   parm                    parccm
069900070411     C                   parm                    parmbr
070000070411     C                   parm      '2'           paropz
070100070411     C*
070200070411     C                   eval      wrkesito = '2'
070300070411     C*
070400070411     C                   seton                                        LR
070500070411     C*
070600070411     C                   ENDSR     '*CANCL'
070700070411     C***
070800070411
070900990910
071000000613     C     *inzsr        BEGSR
071100990910     C*
071200990910     C     *entry        plist
071300990920     C                   parm                    tivlrds
071400990921     C                   parm      wrkesito      esito
071500000724     C                   parm                    prmlit
071600000710     C                   parm                    prmfir
071700000613     C*
071800000830     C* CALCOLA LA DATA CORRENTE
071900000830     C                   time                    wn14             14 0
072000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072100000830     C                   z-add     wn8           g08dat
072200000830     C                   z-add     *zeros        g08inv
072300000830     C                   movel     '0'           g08err
072400000830     C                   call      'XSRDA8'
072500000830     C                   parm                    wlbda8
072600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
072700000830     C*
072800000613     C                   ENDSR
072900000613     C***
