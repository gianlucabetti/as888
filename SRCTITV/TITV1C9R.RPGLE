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
001500070719     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500000830
002600000830     D*------------------
002700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800000830     D*------------------
002900000830     D WLBDA8          DS                  INZ
003000000830     D  G08DAT                 1      8  0
003100000830     D  G08INV                 9     16  0
003200000830     D  G08ERR                17     17
003300000830     D  G08TGI                18     22  0
003400041025     D*------------------
003500041025     D* DS REPERIMENTO NUMERATORE
003600041025     D*------------------
003700041025     D trul33ds      e ds                  inz
003800041025     D*------------------
003900041025     D* DS ARCHITETTURA
004000041025     D*------------------
004100041025     D kpjba         e ds                  inz
004200041025     D*------------------
004300990908
004400010201
004500010201
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900000613     C*
005000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005100070719     C*
005200070719     C* Esegue lancio TISI95R solo x chiusura
005300070719     C                   CLEAR                   TISI95DS
005400070719     C                   EVAL      I95TLA = 'C'
005500070719     C                   CALL      'TISI95R'
005600070719     C                   PARM                    TISI95DS
005700000613     C*
005800010202     C* Effettuo la chiamata al CLLE preposto
005900040506     C                   call(e)   'TITVVTC'
006000010202     C                   parm                    parccm
006100010202     C                   parm                    parmbr
006200010202     C                   parm      '2'           paropz
006300000616     C*
006400010201     C                   seton                                        LR
006500990908
006600000801
006700910830     C*--------------------------------------------------------
006800040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006900910830     C*--------------------------------------------------------
007000040526     C     RWFILE        BEGSR
007100990910     C*
007200990914     C                   if        not %open(tivin00r)
007300990908     C                   open      tivin00r
007400990914     C                   endif
007500021113     C                   if        not %open(fivabwwr)
007600021113     C                   open      fivabwwr
007700990914     C                   endif
007800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007900020305     C                   exsr      prevat
008000010201     C*
008100010202     C                   if        chkcall = '0'
008200010202     C*
008300021113     C                   if        not %open(fivatwwr)
008400021113     C                   open      fivatwwr
008500010201     C                   endif
008600990910     C*
008700010201     C                   clear                   §CTROKVB          5 0
008800020305     C                   clear                   §CTROKVT          5 0
008900000801     C                   clear                   §CTRMO            5 0
009000000801     C                   clear                   §CTRNO            5 0
009100040910     C*
009200921023     C                   DO        *HIVAL
009300990913     C*
009400990915     C                   READ      tivin00r                               70
009500040910     C                   if        vindta > *blanks
009600000613     C                   add       1             rrnum
009700000801     C*
009800000801     C                   if        *in70 = *off
009900000801     C                             and
010000000801     C                             (vinflg = *blanks
010100000801     C                              or vinflg = '0'
010200000801     C                              or vinflg = '2')
010300000801     C*
010400000801     C                   clear                   vinmsg
010500000801     C                   eval      vinflg = '1'
010600040910     C*
010700040910     C* Eseguo routine d traduzione
010800040910     C                   exsr      impvabvat
010900040802     C*
011000010305     C                   endif
011100000905     C*
011200000905     C                   else
011300000905     C                   eval      vinflg = '1'
011400000905     C                   endif
011500000905     C*
011600000905     C  N70              update    tivin000
011700000905     C*
011800991022     C  N70              ENDdo
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
014300990910     C*
014400010201     C                   if        §ctrokvb > 0
014500000724     C                             and vlrpoi <> *zeros
014600010202     C                   exsr      invio
014700990920     C                   endif
014800990920     C*
014900910830     C                   ENDSR
015000000613     C***
015100990920
015200000801     C*----------------------------------------------------*
015300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015400000801     C*----------------------------------------------------*
015500010201     C     INZVAR        BEGSR
015600000801     C*
015700040802     C                   Z-ADD     *zeros        Num5_0            5 0
015800040802     C                   MOVEL     '0'           FlgCAS            1
015900000801     C*
016000000801     C                   ENDSR
016100000801     C*----------------------------------------------------*
016200040910     C*  IMPOSTAZIONE CAMPI COSTANTI
016300000801     C*----------------------------------------------------*
016400000801     C     DEFCAM        BEGSR
016500000801     C*
016600021113     C                   CLEAR                   FIVAB000
016700040802     C                   CLEAR                   FIVAT000
016800020619     C* Imposto i valori di default...
016900120906     C                   Z-ADD     0503318       VABCCM
017000120906     C                   Z-ADD     0503318       VATCCM
017100071210     C                   Z-ADD     050           VABLNP
017200071210     C                   Z-ADD     050           VATLNP
017300070531     C                   Z-ADD     000           VABCTR
017400070531     C                   MOVEL     '7Q'          VABCTM
017500040823     C                   MOVEL     '1'           VABCBO
017600020619     C* ... e poi verifico se sono stati passati come parametri
017700020619     C                   IF        vlrppt > *blanks
017800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
017900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
018000020619     C                   EXSR      CHKNUM
018100020619     C                   IF        PiInt=*on
018200020619     C                   Z-ADD     PiVal         VABCCM
018300020619     C                   Z-ADD     PiVal         VATCCM
018400020619     C                   ENDIF
018500040506     C                   ENDIF
018600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
018700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
018800020619     C                   EXSR      CHKNUM
018900020619     C                   IF        PiInt=*on
019000020619     C                   Z-ADD     PiVal         VABLNP
019100020619     C                   Z-ADD     PiVal         VATLNP
019200040506     C                   ENDIF
019300020619     C                   ENDIF
019400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
019500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
019600020619     C                   EXSR      CHKNUM
019700020619     C                   IF        PiInt=*on
019800020619     C                   Z-ADD     PiVal         VABCTR
019900040506     C                   ENDIF
020000020619     C                   ENDIF
020100060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
020200060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
020300060202     C                   ENDIF
020400020619     C                   ENDIF
020500000801     C*
020600000801     C                   ENDSR
020700000801     C*----------------------------------------------------*
020800040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
020900000801     C*----------------------------------------------------*
021000040910     C     IMPVABVAT     BEGSR
021100040910     C*
021200040910     C* Traduzione relativa ai tipi record del file del cliente
021300040910     C*
021400040910     C***
021500071210     C* ...tipo record 'ECE' (testata - partner ID)
021600071210     C                   IF        %subst(vindta:1:3) = 'ECE'
021700071210     C* PartnerID (VABCCM)
021800071210     C                   MOVEL     *blanks       wPtnrID          10
021900071210     C                   EVAL      wPtnrID=%trim(%subst(vindta:280:10))
022000071210     C*
022100071210     C                   ENDIF
022200071210     C*
022300071210     C***
022400071210     C* ...tipo record 'EBE' (testata)
022500071210     C                   IF        %subst(vindta:1:3) = 'EBE'
022600071210     C* Resetto indicatore d anomalia sul singolo record
022700071210     C                   eval      vinflg = '1'
022800071210     C* ......inizializzazioni iniziali e formati record file Bartolini
022900071210     C                   EXSR      INZVAR
023000071210     C                   EXSR      DEFCAM
023100071210     C*
023200071210     C                   Z-ADD     *zeros        errore            1 0
023300071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
023400071210     C                   MOVEL     datcor        VABAAS
023500071210     C                   MOVEL     datcor        VATAAS
023600071210     C                   MOVE      datcor        VABMGS
023700071210     C                   MOVE(P)   vlrpoi        VABFGS
023800071210     C                   MOVE(P)   vlrpoi        VATFGS
023900071210     C* ......VABNSP/VATNSP
024000071210     C* NSP => Stacco un numeratore da AZNUM
024100071210     C                   clear                   TRUL33DS
024200071210     C                   eval      I33OPE = *zeros
024300071210     C                   eval      I33CNU = 302
024400071210     C                   eval      I33NUM = 1
024500071210     C                   movel     TRUL33DS      KPJBU
024600071210     C                   call      'TRUL33R'
024700071210     C                   parm                    KPJBA
024800071210     C                   movel     KPJBU         TRUL33DS
024900071210     C                   if        O33ERR = *zeros
025000071210     C                   z-add     O33NRF        VABNSP
025100071210     C                   z-add     O33NRF        VATNSP
025200071210     C                   else
025300071210     C                   Z-ADD     1             errore
025400071210     C                   EVAL      vinmsg = %trimr(vinmsg)
025500071210     C                             + ' ' + 'VABNSP VATNSP'
025600071210     C                   endif
025700071210     C* ......VABNOT
025800071213     C                   EVAL      VABNOT='ORD CLI: ' +
025900071213     C                                    %trim(%subst(vindta:393:25))
026000080211     C* ......VABNT2
026100080211     C                   EVAL      VABNT2='CUST ACCOUNT #: ' +
026200080211     C                                    %trim(%subst(vindta:9:13))
026300060202     C* ......VABRMN
026400071210     C                   EVAL      PiStr=%trim(%subst(vindta:22:15))
026500060202     C                   EXSR      CHKNUM
026600060202     C                   IF        PiInt=*on
026700060202     C                   Z-ADD     PiVal         VABRMN
026800060202     C                   ELSE
026900060202     C                   ADD       1             errore
027000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
027100060202     C                             + ' ' + 'VABRMN'
027200060202     C                   ENDIF
027300060202     C* ......VABPKB
027400071210     C                   EVAL      PiStr=%trim(%subst(vindta:285:9))
027500060202     C                   EXSR      CHKNUM
027600060202     C                   IF        PiNum=*on
027700070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
027800070726     C                   Z-ADD(H)  PiVal         VABPKB
027900060202     C                   ELSE
028000060202     C                   ADD       1             errore
028100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028200060202     C                             + ' ' + 'VABPKB'
028300060202     C                   ENDIF
028400071210     C* ......VABVLB
028500071210     C*                  EVAL      PiStr=%trim(%subst(vindta:294:9))
028600071210     C*                  EXSR      CHKNUM
028700071210     C*                  IF        PiNum=*on
028800071210     C*                  EVAL      PiVal = PiVal / 1000                         da dm3 a m3
028900071210     C*                  Z-ADD(H)  PiVal         VABVLB
029000071210     C*                  ELSE
029100071210     C*                  ADD       1             errore
029200071210     C*                  EVAL      vinmsg = %trimr(vinmsg)
029300071210     C*                            + ' ' + 'VABVLB'
029400071210     C*                  ENDIF
029500071210     C* ......VABRMA
029600071210     C                   EVAL      VABRMA=%trim(%subst(vindta:22:15))
029700060202     C* ......VABRSD
029800071210     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
029900060202     C* ......VABIND
030000071210     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))+
030100071210     C                                    %trim(%subst(vindta:158:35))+
030200071210     C                                    %trim(%subst(vindta:193:35))
030300071210     C* ......VABRD2
030400071210     C                   EVAL      VABRD2=%trim(%subst(vindta:88:35))
030500060202     C* ......VABLOD
030600070719     C                   EVAL      VABLOD=%trim(%subst(vindta:237:35))
030700060202     C* ......VABCAD
030800070719     C                   EVAL      PiStr=%trim(%subst(vindta:228:9))
030900060202     C                   EXSR      CHKNUM
031000060202     C                   IF        PiInt=*on
031100060202     C                   Z-ADD     PiVal         Num5_0
031200060202     C                   MOVEL(P)  Num5_0        VABCAD
031300060202     C                   ELSE
031400060202     C                   ADD       1             errore
031500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
031600060202     C                             + ' ' + 'VABCAD'
031700060202     C                   ENDIF
031800060202     C* ......VABPRD
031900070719     C* Reperisco la provincia dal CAP e dalla località
032000070719     C                   IF        VABPRD  = *blanks AND
032100070719     C                             VABCAD <> *blanks
032200070719     C                   CLEAR                   TISI95DS
032300070719     C                   EVAL      I95TCN = '3'
032400070719     C                   Z-ADD     datcor        I95DAT
032500070719     C                   EVAL      I95CAP = VABCAD
032600070719     C                   EVAL      I95LOC = VABLOD
032700070719     C                   CALL      'TISI95R'
032800070719     C                   PARM                    TISI95DS
032900070719     C                   EVAL      VABPRD = O95PRV
033000070719     C                   ENDIF
033100090219     C* Verifico se colli pallettizzati (bancali)
033200071210     C                   IF        %trim(%subst(vindta:418:4))='PLD'
033300071210     C* ......VABCTR
033400071210     C                   EVAL      VABCTR=050
033500071210     C* ......VABNCL
033600071210     C                   EVAL      PiStr=%trim(%subst(vindta:275:5))
033700071210     C                   EXSR      CHKNUM
033800071210     C                   IF        PiInt=*on
033900071210     C                   Z-ADD     PiVal         VABNCL
034000071210     C                   ELSE
034100071210     C                   ADD       1             errore
034200071210     C                   EVAL      vinmsg = %trimr(vinmsg)
034300071210     C                             + ' ' + 'VABNCL'
034400071210     C                   ENDIF
034500071210     C* ......VABNAS
034600071210     C                   EVAL      VABNAS='Colli:'+%trim(%subst(vindta:280:5))
034700071210     C* ......VABCCM
034800090122     C                   IF        wPtnrID = '0000065447'
034900090122     C                   EVAL      VABCCM=0503318
035000090122     C                   EVAL      VATCCM=0503318
035100090122     C                   ENDIF
035200090122     C                   IF        wPtnrID = '0000068252'
035300090122     C                   EVAL      VABCCM=0503318
035400090122     C                   EVAL      VATCCM=0503318
035500090122     C                   ENDIF
035600090122     C                   IF        wPtnrID = '0000068628'
035700090122     C                   EVAL      VABCCM=0503318
035800090122     C                   EVAL      VATCCM=0503318
035900090122     C                   ENDIF
036000090122     C                   IF        wPtnrID = '0000069190'
036100090122     C                   EVAL      VABCCM=0503318
036200090122     C                   EVAL      VATCCM=0503318
036300090122     C                   ENDIF
036400071210     C                   IF        wPtnrID = '0000056246'
036500071210     C                   EVAL      VABCCM=0503318
036600071210     C                   EVAL      VATCCM=0503318
036700071210     C                   ENDIF
036800090122     C*                  IF        wPtnrID = '0000062536'
036900090122     C*                  EVAL      VABCCM=0503318
037000090122     C*                  EVAL      VATCCM=0503318
037100090122     C*                  ENDIF
037200071210     C                   IF        wPtnrID = '0000062656'
037300071210     C                   EVAL      VABCCM=0503318
037400071210     C                   EVAL      VATCCM=0503318
037500071210     C                   ENDIF
037600071210     C                   IF        wPtnrID = '0000062655'
037700071210     C                   EVAL      VABCCM=0503318
037800071210     C                   EVAL      VATCCM=0503318
037900071210     C                   ENDIF
038000090122     C*                  IF        wPtnrID = '0000062538'
038100090122     C*                  EVAL      VABCCM=0503318
038200090122     C*                  EVAL      VATCCM=0503318
038300090122     C*                  ENDIF
038400071210     C                   IF        wPtnrID = '0000041250'
038500071210     C                   EVAL      VABCCM=0503500
038600071210     C                   EVAL      VATCCM=0503500
038700071210     C                   ENDIF
038800090219     C                   IF        wPtnrID = '0000054576'
038900090219     C                   EVAL      VABCCM=0503318
039000090219     C                   EVAL      VATCCM=0503318
039100090219     C                   ENDIF
039200090414     C                   IF        wPtnrID = '0000070815'
039300090414     C                   EVAL      VABCCM=0504221
039400090414     C                   EVAL      VATCCM=0504221
039500090414     C                   ENDIF
039600071210     C                   IF        wPtnrID = *blanks
039700071210     C                   EVAL      VABCCM=0503501
039800071210     C                   EVAL      VATCCM=0503501
039900071210     C                   ENDIF
040000071210     C                   ENDIF
040100071210     C*
040200071210     C                   IF        %trim(%subst(vindta:418:4))='PKD'
040300071210     C* ......VABCTR
040400071210     C                   EVAL      VABCTR=000
040500071210     C* ......VABNCL
040600071210     C                   EVAL      PiStr=%trim(%subst(vindta:275:5))
040700071210     C                   EXSR      CHKNUM
040800071210     C                   IF        PiInt=*on
040900071210     C                   Z-ADD     PiVal         VABNCL
041000071210     C                   ELSE
041100071210     C                   ADD       1             errore
041200071210     C                   EVAL      vinmsg = %trimr(vinmsg)
041300071210     C                             + ' ' + 'VABNCL'
041400071210     C                   ENDIF
041500071210     C* ......VABCCM
041600071210     C                   IF        wPtnrID = '0000056246'
041700071210     C                   EVAL      VABCCM=0503318
041800071210     C                   EVAL      VATCCM=0503318
041900071210     C                   ENDIF
042000090122     C                   IF        wPtnrID = '0000065447'
042100090122     C                   EVAL      VABCCM=0503318
042200090122     C                   EVAL      VATCCM=0503318
042300090122     C                   ENDIF
042400090122     C                   IF        wPtnrID = '0000068252'
042500090122     C                   EVAL      VABCCM=0503318
042600090122     C                   EVAL      VATCCM=0503318
042700090122     C                   ENDIF
042800090122     C                   IF        wPtnrID = '0000068628'
042900090122     C                   EVAL      VABCCM=0503318
043000090122     C                   EVAL      VATCCM=0503318
043100090122     C                   ENDIF
043200090122     C*                  IF        wPtnrID = '0000062536'
043300090122     C*                  EVAL      VABCCM=0503318
043400090122     C*                  EVAL      VATCCM=0503318
043500090122     C*                  ENDIF
043600090122     C*                  IF        wPtnrID = '0000062538'
043700090122     C*                  EVAL      VABCCM=0503318
043800090122     C*                  EVAL      VATCCM=0503318
043900090122     C*                  ENDIF
044000071210     C                   IF        wPtnrID = '0000041250'
044100120906     C***                EVAL      VABCCM=0503498
044200120906     C***                EVAL      VATCCM=0503498
044300120906     C                   EVAL      VABCCM=0503318
044400120906     C                   EVAL      VATCCM=0503318
044500071210     C                   ENDIF
044600090219     C                   IF        wPtnrID = '0000054576'
044700090219     C                   EVAL      VABCCM=0503318
044800090219     C                   EVAL      VATCCM=0503318
044900090219     C                   ENDIF
045000071210     C                   IF        wPtnrID = *blanks
045100071210     C                   EVAL      VABCCM=0503499
045200071210     C                   EVAL      VATCCM=0503499
045300071210     C                   ENDIF
045400071210     C                   ENDIF
045500060202     C*
045600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045700060202     C                   IF        FlgCAS <> '0'
045800060202     C                   IF        VABCBO = '1'
045900060202     C                   EVAL      VABCBO = '4'
046000060202     C                   ELSE
046100060202     C                   EVAL      VABCBO = '6'
046200060202     C                   ENDIF
046300060202     C                   ENDIF
046400060202     C*
046500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046600060202     C                   EXSR      CHKIMPDIV
046700060202     C* ...e in fine...
046800060202     C                   WRITE     FIVAB000
046900040910     C*
047000040910     C                   ENDIF
047100040910     C***
047200070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
047300070719     C                   IF        %subst(vindta:1:3) = 'EBC'
047400070719     C* Resetto indicatore d anomalia sul singolo record
047500070719     C                   eval      vinflg = '1'
047600040910     C* ......VATNOT
047700071210     C                   EVAL      VATNOT=%trim(%subst(vindta:48:35))
047800040910     C                   EVAL      VATTRC = 'E'
047900070719     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
048000060202     C                   IF        VATNOT <> *blanks
048100040910     C                   WRITE     FIVAT000
048200060202     C                   ENDIF
048300040910     C*
048400040910     C                   ENDIF
048500010202     C*
048600000801     C* Ebbene...
048700000801     C                   ADD       1             §CTRMO
048800010201     C                   IF        errore <> *zeros
048900000801     C                   ADD       1             §CTRNO
049000000801     C                   EVAL      vinflg = '2'
049100000801     C                   ELSE
049200010201     C                   ADD       1             §CTROKVB
049300000801     C                   ENDIF
049400000801     C*
049500000801     C                   ENDSR
049600010202     C*----------------------------------------------------*
049700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049800010202     C*----------------------------------------------------*
049900020305     C     PREVAT        BEGSR
050000010202     C*
050100021113     C* Compongo il nome del membro da dare al FIVATWWR
050200010202     C                   eval      parmbr = vlrhdl
050300010202     C                   movel     'M'           parmbr
050400060113     C                   eval      parccm = vlrksc
050500010202     C                   eval      paropz = '1'
050600010202     C* Effettuo la chiamata al CLLE preposto
050700040506     C                   call(e)   'TITVVTC'
050800010202     C                   parm                    parccm
050900010202     C                   parm                    parmbr
051000010202     C                   parm                    paropz
051100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051200010202     C                   if        %error
051300010202     C                   movel     '1'           chkcall
051400010202     C                   else
051500010202     C                   movel     '0'           chkcall
051600010202     C                   endif
051700010202     C*
051800010202     C                   ENDSR
051900000801     C*----------------------------------------------------*
052000000801     C*  CONTROLLO NUMERICITA' CAMPI
052100000801     C*----------------------------------------------------*
052200000801     C     CHKNUM        BEGSR
052300000801     C*
052400000801     C                   call(e)   'ISNUMERIC'
052500000801     C                   PARM                    PiStr            30
052600040714     C                   PARM      ','           PiDecChr          1
052700000801     C                   PARM      *ZEROS        PiVal            30 9
052800000801     C                   PARM      '0'           PiInt             1
052900000801     C                   PARM      '0'           PiNum             1
053000000801     C                   IF        %error
053100000801     C                   EVAL      PiInt=*off
053200000801     C                   ENDIF
053300000801     C*
053400000801     C                   ENDSR
053500000801     C***
053600000801
053700011113
053800011113     C*----------------------------------------------------*
053900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
054000011113     C*----------------------------------------------------*
054100011113     C     CHKIMPDIV     BEGSR
054200011113     C*
054300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
054400011113     C                   Z-ADD     *zeros        wrkDec            9 9
054500011113     C*
054600011113     C* Come prima cosa effettuo considerazioni sulla divisa
054700011113     C                   IF        vabIAS > *zeros
054800011113     C                   IF        vabVAS <> 'EUR'
054900011113     C                   EVAL      vabVAS =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabCAS > *zeros
055400011113     C                   IF        vabVCA <> 'EUR'
055500011113     C                   EVAL      vabVCA =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C                   IF        vabVMD > *zeros
056000020305     C                   IF        vabVAD <> 'EUR'
056100011113     C                   EVAL      vabVAD =  'ITL'
056200011113     C                   ENDIF
056300011113     C                   ENDIF
056400011113     C*
056500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056600011113     C                   Z-ADD     vabIAS        wrkDec
056700011113     C                   IF        wrkDec > *zeros
056800011113     C                   IF        vabVAS = 'ITL'
056900011113     C                   EVAL      vabIAS = *zeros
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
057400011113     C                   Z-ADD     vabCAS        wrkDec
057500011113     C                   IF        wrkDec > *zeros
057600011113     C                   IF        vabVCA = 'ITL'
057700011113     C                   EVAL      vabCAS = *zeros
057800011113     C                   ENDIF
057900011113     C                   ENDIF
058000011113     C*
058100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
058200011113     C                   Z-ADD     vabVMD        wrkDec
058300011113     C                   IF        wrkDec > *zeros
058400011113     C                   IF        vabVAD = 'ITL'
058500011113     C                   EVAL      vabVMD = *zeros
058600011113     C                   ENDIF
058700011113     C                   ENDIF
058800011113     C*
058900011113     C                   ENDSR
059000011113     C***
059100011113
059200011113
059300000801
059400000801
059500990920      /TITLE Invio dei dati al punto operativo.
059600010202     C     invio         BEGSR
059700990920     C*
059800021113     C* 1° invio FIVAT
059900010201     C                   reset                   dscmz
060000010201     C                   move      vlrpoi        cmzdst
060100021113     C                   eval      cmzfld = 'FIVATWWR'
060200010201     C                   eval      cmzmbd = vlrhdl
060300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060400021009     C***                if        prmfir = *blanks
060500021113     C                   eval      cmzfla = 'FIVAT00F'
060600021113     C                   eval      cmzmba = 'FIVAT00F'
060700021009     C***                else
060800021009     C***                eval      cmzfla = prmfir
060900021009     C***                eval      cmzmba = prmfir
061000021009     C***                endif
061100010201     C                   eval      cmznrr = *zeros
061200020305     C                   move      §ctrokvt      cmznrr
061300021018     C                   eval      cmzlba = vlrfl1
061400010201     C                   call(e)   'TIS711C'
061500010201     C                   parm                    dscmz
061600010201     C                   parm      *blanks       esito
061700010205     C                   if        %error
061800010205     C                             or cmzerr = '1'
061900010205     C                             or esito  = '1'
062000010205     C                   eval      wrkesito = '3'
062100010205     C                   else
062200010201     C*
062300021113     C* 2° invio FIVAB
062400010201     C                   reset                   dscmz
062500010201     C                   move      vlrpoi        cmzdst
062600010201     C                   eval      cmzfld = vlrfou
062700010201     C                   eval      cmzmbd = vlrhdl
062800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062900021009     C***                if        prmfir = *blanks
063000021113     C                   eval      cmzfla = 'FIVAB00F'
063100021113     C                   eval      cmzmba = 'FIVAB00F'
063200021009     C***                else
063300021009     C***                eval      cmzfla = prmfir
063400021009     C***                eval      cmzmba = prmfir
063500021009     C***                endif
063600010201     C                   eval      cmznrr = *zeros
063700010201     C                   move      §ctrokvb      cmznrr
063800021018     C                   eval      cmzlba = vlrfl1
063900010201     C                   call(e)   'TIS711C'
064000010201     C                   parm                    dscmz
064100010201     C                   parm      *blanks       esito
064200010201     C                   if        %error
064300010201     C                             or cmzerr = '1'
064400010201     C                             or esito  = '1'
064500010201     C                   eval      wrkesito = '3'
064600010201     C                   endif
064700010205     C                   endif
064800990920     C*
064900000613     C                   ENDSR
065000000613     C***
065100070411
065200070411     C     *pssr         BEGSR
065300070411     C*
065400070411     C                   if        %open(tivin00r)
065500070411     C                   close     tivin00r
065600070411     C                   endif
065700070411     C                   if        %open(fivabwwr)
065800070411     C                   close     fivabwwr
065900070411     C                   endif
066000070411     C                   if        %open(fivatwwr)
066100070411     C                   close     fivatwwr
066200070411     C                   endif
066300070411     C*
066400070411     C* Effettuo la chiamata al CLLE preposto
066500070411     C                   call(e)   'TITVVTC'
066600070411     C                   parm                    parccm
066700070411     C                   parm                    parmbr
066800070411     C                   parm      '2'           paropz
066900070411     C*
067000070411     C                   eval      wrkesito = '2'
067100070411     C*
067200070411     C                   seton                                        LR
067300070411     C*
067400070411     C                   ENDSR     '*CANCL'
067500070411     C***
067600070411
067700990910
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500000613     C*
068600000830     C* CALCOLA LA DATA CORRENTE
068700000830     C                   time                    wn14             14 0
068800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068900000830     C                   z-add     wn8           g08dat
069000000830     C                   z-add     *zeros        g08inv
069100000830     C                   movel     '0'           g08err
069200000830     C                   call      'XSRDA8'
069300000830     C                   parm                    wlbda8
069400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069500000830     C*
069600000613     C                   ENDSR
069700000613     C***
