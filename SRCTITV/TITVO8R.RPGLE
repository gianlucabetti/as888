000100051102      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400051028     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600051102     FEDIVABwr  O    E             DISK    usropn
000700051102     FEDIVATwr  O    E             DISK    usropn
000800041210     D*------------
000900041210     D* SCHIERE
001000041210     D*------------
001100051028     D skNAZISO        S              3    DIM(2000)
001200051028     D skNAZBAR        S              3    DIM(2000)
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900051028     D tivlrds       e ds                  extname(tivlr00f)
002000051028     D ds15          e ds
002100051028     D tisi95ds      e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700041210     D parccm          s              8    INZ(*blanks)
002800010202     D parmbr          s             10    INZ(*blanks)
002900010202     D paropz          s              1    INZ(*blanks)
003000010202     D chkcall         s              1    INZ(*blanks)
003100050805     D FlgNewBol       s              1    INZ(*blanks)
003200051028     D jNAZ            s              5  0 INZ(*zeros)
003300000830
003400000830     D*------------------
003500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600000830     D*------------------
003700000830     D WLBDA8          DS                  INZ
003800000830     D  G08DAT                 1      8  0
003900000830     D  G08INV                 9     16  0
004000000830     D  G08ERR                17     17
004100000830     D  G08TGI                18     22  0
004200041025     D*------------------
004300041025     D* DS REPERIMENTO NUMERATORE
004400041025     D*------------------
004500051028     D trul33ds      e ds                  inz
004600041025     D*------------------
004700041025     D* DS ARCHITETTURA
004800041025     D*------------------
004900051028     D kpjba         e ds                  inz
005000041025     D*------------------
005100990908
005200010201
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800041210     C                   EXSR      CARTAB                                       CARICA TABELLE
005900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
006000000613     C*
006100010202     C* Effettuo la chiamata al CLLE preposto
006200051102     C                   call(e)   'TITVEVTC'
006300010202     C                   parm                    parccm
006400010202     C                   parm                    parmbr
006500010202     C                   parm      '2'           paropz
006600051028     C*
006700051028     C* Effettuo lancio TISI95 solo x chiusura
006800051028     C                   CLEAR                   TISI95DS
006900051028     C                   EVAL      I95TLA = 'C'
007000051028     C                   CALL      'TISI95R'
007100051028     C                   PARM                    TISI95DS
007200000616     C*
007300010201     C                   seton                                        LR
007400041210
007500041210     C*--------------------------------------------------------
007600041210     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
007700041210     C*--------------------------------------------------------
007800051028     C     CARTAB        BEGSR
007900041210     C*
008000041210     C* TABELLA '15' - NAZIONI
008100041210     C                   clear                   skNAZISO
008200041210     C                   clear                   skNAZBAR
008300041210     C                   eval      tblKUT = 1
008400041210     C                   eval      tblCOD = '15'
008500041210     C     KEYtabP       setll     tabel00f
008600041210     C     KEYtabP       reade     tabel00f
008700041210     C                   dow       not %eof(tabel00f)
008800041210     C                   if        tblFLG = *blanks
008900041210     C                   movel(p)  tblUNI        ds15
009000041210     C                   add       1             jNAZ
009100041210     C                   eval      skNAZBAR(jNAZ) = tblKEY
009200041210     C                   eval      skNAZISO(jNAZ) = §15COD
009300041210     C                   endif
009400041210     C     KEYtabP       reade     tabel00f
009500041210     C                   enddo
009600041210     C*
009700041210     C                   ENDSR
009800041210     C***
009900041210
010000041210
010100041210
010200910830     C*--------------------------------------------------------
010300051102     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR  *
010400910830     C*--------------------------------------------------------
010500040526     C     RWFILE        BEGSR
010600990910     C*
010700990914     C                   if        not %open(tivin00r)
010800990908     C                   open      tivin00r
010900990914     C                   endif
011000051102     C                   if        not %open(edivabwr)
011100051102     C                   open      edivabwr
011200990914     C                   endif
011300051102     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011400020305     C                   exsr      prevat
011500010201     C*
011600010202     C                   if        chkcall = '0'
011700010202     C*
011800051102     C                   if        not %open(edivatwr)
011900051102     C                   open      edivatwr
012000010201     C                   endif
012100990910     C*
012200010201     C                   clear                   §CTROKVB          5 0
012300020305     C                   clear                   §CTROKVT          5 0
012400000801     C                   clear                   §CTRMO            5 0
012500000801     C                   clear                   §CTRNO            5 0
012600040910     C*
012700921023     C                   DO        *HIVAL
012800990913     C*
012900990915     C                   READ      tivin00r                               70
013000040910     C                   if        vindta > *blanks
013100000613     C                   add       1             rrnum
013200000801     C*
013300000801     C                   if        *in70 = *off
013400000801     C                             and
013500000801     C                             (vinflg = *blanks
013600000801     C                              or vinflg = '0'
013700000801     C                              or vinflg = '2')
013800000801     C*
013900000801     C                   clear                   vinmsg
014000000801     C                   eval      vinflg = '1'
014100040910     C*
014200040910     C* Eseguo routine d traduzione
014300040910     C                   exsr      impvabvat
014400040802     C*
014500010305     C                   endif
014600000905     C*
014700000905     C                   else
014800000905     C                   eval      vinflg = '1'
014900000905     C                   endif
015000000905     C*
015100000905     C  N70              update    tivin000
015200000905     C*
015300991022     C  N70              ENDdo
015400041210     C*
015500050805     C* Scarico i buffer eventualmente rimasti in sospeso
015600051028     C                   IF        FlgNewBol = '1'
015700051102     C*
015800051102     C* VALORIZZO CAMPI RELATIVI AL "CMR" DELLA TESTATA
015900051102     C                   EXSR      VALCMRVAB
016000051102     C*
016100051102     C* SCARICO I BUFFER DELLA TESTATA
016200051102     C                   WRITE     EDIVAB00
016300041210     C                   ENDIF
016400010202     C*
016500010202     C                   endif
016600990910
016700990910     C* Se non ci sono record con errori ...
016800000710     C                   if        §ctrno = 0
016900990910     C* ... restituisco esito OK.
017000990921     C                   eval      wrkesito = '0'
017100990910     C                   else
017200010201     C                   if        §ctrokvb > 0
017300990921     C                   eval      wrkesito = '1'
017400000710     C                   else
017500000710     C                   eval      wrkesito = '2'
017600990910     C                   endif
017700000710     C                   endif
017800990910     C*
017900990914     C                   if        %open(tivin00r)
018000990908     C                   close     tivin00r
018100990914     C                   endif
018200051102     C                   if        %open(edivabwr)
018300051102     C                   close     edivabwr
018400990914     C                   endif
018500051102     C                   if        %open(edivatwr)
018600051102     C                   close     edivatwr
018700010201     C                   endif
018800990910     C*
018900010201     C                   if        §ctrokvb > 0
019000000724     C                             and vlrpoi <> *zeros
019100010202     C                   exsr      invio
019200990920     C                   endif
019300990920     C*
019400910830     C                   ENDSR
019500000613     C***
019600051102
019700051102     C*----------------------------------------------------*
019800051102     C*  VALORIZZAZIONE CAMPI RELATIVI AL "CMR" FILE EDIVAB
019900051102     C*----------------------------------------------------*
020000051102     C     VALCMRVAB     BEGSR
020100051102     C*
020200051102     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
020300051102     C                   EVAL      VABCMR = datoracor
020400051102     C                   EVAL      VABDCM = DATCOR
020500051102     C                   EVAL      VABDTS = DATCOR
020600051102     C                   EVAL      VABHMS = ORACOR
020700051102     C                   EVAL      VABCNT = 1
020800051102     C*
020900051102     C                   ENDSR
021000051102     C***
021100051102
021200051102     C*----------------------------------------------------*
021300051102     C*  VALORIZZAZIONE CAMPI RELATIVI AL "CMR" FILE EDIVAT
021400051102     C*----------------------------------------------------*
021500051102     C     VALCMRVAT     BEGSR
021600051102     C*
021700051102     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
021800051102     C                   EVAL      VATCMR = datoracor
021900051102     C                   EVAL      VATCNT = 1
022000051102     C*
022100051102     C                   ENDSR
022200051102     C***
022300990920
022400000801     C*----------------------------------------------------*
022500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600000801     C*----------------------------------------------------*
022700010201     C     INZVAR        BEGSR
022800000801     C*
022900040802     C                   Z-ADD     *zeros        Num5_0            5 0
023000051028     C                   MOVEL     '0'           FlgCAS            1
023100000801     C*
023200000801     C                   ENDSR
023300000801     C*----------------------------------------------------*
023400040910     C*  IMPOSTAZIONE CAMPI COSTANTI
023500000801     C*----------------------------------------------------*
023600000801     C     DEFCAM        BEGSR
023700000801     C*
023800051102     C                   CLEAR                   EDIVAB00
023900051102     C                   CLEAR                   EDIVAT00
024000020619     C* Imposto i valori di default...
024100150120     C                   Z-ADD     0433177       VABCCM
024200150120     C                   Z-ADD     0433177       VATCCM
024300051111     C                   Z-ADD     043           VABLNP
024400051111     C                   Z-ADD     043           VATLNP
024500080313     C***                Z-ADD     001           VABCTR
024600080313     C                   Z-ADD     000           VABCTR
024700051111     C                   MOVEL     '1'           VABCBO
024800051111     C                   MOVEL     '7Q'          VABCTM
024900020619     C* ... e poi verifico se sono stati passati come parametri
025000020619     C                   IF        vlrppt > *blanks
025100040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025300020619     C                   EXSR      CHKNUM
025400020619     C                   IF        PiInt=*on
025500020619     C                   Z-ADD     PiVal         VABCCM
025600020619     C                   Z-ADD     PiVal         VATCCM
025700020619     C                   ENDIF
025800040506     C                   ENDIF
025900040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026100020619     C                   EXSR      CHKNUM
026200020619     C                   IF        PiInt=*on
026300020619     C                   Z-ADD     PiVal         VABLNP
026400020619     C                   Z-ADD     PiVal         VATLNP
026500040506     C                   ENDIF
026600020619     C                   ENDIF
026700040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
026800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026900020619     C                   EXSR      CHKNUM
027000020619     C                   IF        PiInt=*on
027100020619     C                   Z-ADD     PiVal         VABCTR
027200040506     C                   ENDIF
027300020619     C                   ENDIF
027400041210     C                   IF        %subst(vlrppt:14:2) <> *blanks
027500041210     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
027600041210     C                   ENDIF
027700020619     C                   ENDIF
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100051102     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB e EDIVAT)
028200000801     C*----------------------------------------------------*
028300040910     C     IMPVABVAT     BEGSR
028400041210     C*
028500041210     C* Innanzitutto ad ogni record da tradurre inizializzo il flag d errore traduzione
028600041210     C                   Z-ADD     *zeros        errore            1 0
028700040910     C*
028800041210     C********
028900051028     C* Tipo record 'KOPF' (dati destinatario e riferimenti)
029000041210     C********
029100051028     C                   IF        %trim(%subst(vindta:1:4)) = 'KOPF'
029200051102     C* ......se già effettuata 1 precedente valorizzazione bolla scarico il buffer del EDIVAB
029300041210     C                   IF        FlgNewBol = '1'
029400051102     C*
029500051102     C* VALORIZZO CAMPI RELATIVI AL "CMR" DELLA TESTATA
029600051102     C                   EXSR      VALCMRVAB
029700051102     C*
029800051102     C* SCARICO I BUFFER DELLA TESTATA
029900051102     C                   WRITE     EDIVAB00
030000041210     C                   EVAL      FlgNewBol = '0'
030100041210     C                   ENDIF
030200041210     C* ......inizializzazioni iniziali e formati record file Bartolini x valorizzazione nuova bolla
030300040910     C                   EXSR      INZVAR
030400040910     C                   EXSR      DEFCAM
030500041210     C                   EVAL      FlgNewBol = '1'
030600040928     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
030700040928     C                   MOVEL     datcor        VABAAS
030800040928     C                   MOVEL     datcor        VATAAS
030900040928     C                   MOVE      datcor        VABMGS
031000040928     C                   MOVE(P)   vlrpoi        VABFGS
031100040928     C                   MOVE(P)   vlrpoi        VATFGS
031200041210     C* ......VABNSP/VATNSP => Stacco un numeratore da AZNUM
031300041210     C                   CLEAR                   TRUL33DS
031400041210     C                   EVAL      I33OPE = *zeros
031500041210     C                   EVAL      I33CNU = 302
031600041210     C                   EVAL      I33NUM = 1
031700041210     C                   MOVEL     TRUL33DS      KPJBU
031800041210     C                   CALL      'TRUL33R'
031900041210     C                   PARM                    KPJBA
032000041210     C                   MOVEL     KPJBU         TRUL33DS
032100041210     C                   IF        O33ERR = *zeros
032200041210     C                   Z-ADD     O33NRF        VABNSP
032300041210     C                   Z-ADD     O33NRF        VATNSP
032400041210     C                   ELSE
032500041210     C                   ADD       1             errore
032600041210     C                   EVAL      vinmsg = %trimr(vinmsg)
032700041025     C                             + ' ' + 'VABNSP VATNSP'
032800041210     C                   ENDIF
032900051028     C* ......VABRMN
033000051028     C                   EVAL      PiStr=%trim(%subst(vindta:6:10))
033100051028     C                   EXSR      CHKNUM
033200051028     C                   IF        PiInt=*on
033300051028     C                   Z-ADD     PiVal         VABRMN
033400051028     C                   ELSE
033500051028     C                   Z-ADD     1             VABRMN
033600051028     C                   ADD       1             errore
033700051028     C                   EVAL      vinmsg = %trimr(vinmsg)
033800051028     C                             + ' ' + 'VABRMN'
033900051028     C                   ENDIF
034000050805     C* ......VABRMA
034100051129     C                   EVAL      VABRMA=%trim(%subst(vindta:38:20))+' '+
034200051129     C                                    %trim(%subst(vindta:59:20))
034300051129     C*                  Z-ADD     *zeros        wRMA_Part2_Num   20 0
034400051129     C*                  MOVEL     *blanks       wRMA_Part2_Alf   20
034500051129     C*                  EVAL      wRMA_Part2_Alf = %subst(vindta:38:20)
034600051129     C*                  MOVE(P)   wRMA_Part2_AlfwRMA_Part2_Num
034700051129     C*                  EVAL      VABRMA=%trim(%subst(vindta:17:20)) + ' ' +
034800051129     C*                            %trim(%editc(wRMA_Part2_Num:'4'))
034900050805     C* ......VABRSD/VABRD2
035000051028     C                   MOVEL     *blanks       wRAGSOCDEST     140
035100080313     C                   EVAL      wRAGSOCDEST=%trim(%subst(vindta:080:35))
035200080313     C*
035300080313     C                   IF        %trim(%subst(vindta:116:35)) <>
035400080313     C                             %trim(%subst(vindta:080:35))
035500080313     C                   EVAL      wRAGSOCDEST=%trim(wRAGSOCDEST)+' '+
035600080313     C                                         %trim(%subst(vindta:116:35))
035700080313     C                   ENDIF
035800080313     C*
035900080313     C                   IF        %trim(%subst(vindta:152:35)) <>
036000080313     C                             %trim(%subst(vindta:080:35)) AND
036100080313     C                             %trim(%subst(vindta:152:35)) <>
036200080313     C                             %trim(%subst(vindta:116:35))
036300080313     C                   EVAL      wRAGSOCDEST=%trim(wRAGSOCDEST)+' '+
036400080313     C                                         %trim(%subst(vindta:152:35))
036500080313     C                   ENDIF
036600080313     C*
036700090320     C***                IF        %trim(%subst(vindta:188:35)) <>
036800090320     C***                          %trim(%subst(vindta:080:35)) AND
036900090320     C***                          %trim(%subst(vindta:188:35)) <>
037000090320     C***                          %trim(%subst(vindta:116:35)) AND
037100090320     C***                          %trim(%subst(vindta:188:35)) <>
037200090320     C***                          %trim(%subst(vindta:152:35))
037300090320     C***                EVAL      wRAGSOCDEST=%trim(wRAGSOCDEST)+' '+
037400090320     C***                                      %trim(%subst(vindta:188:35))
037500090320     C***                ENDIF
037600090320     C*
037700050805     C                   EVAL      VABRSD=%trim(%subst(wRAGSOCDEST:1:35))
037800050805     C                   EVAL      VABRD2=%trim(%subst(wRAGSOCDEST:36:35))
037900090320     C* ......VABNT2
038000090320     C                   EVAL      VABNT2=%trim(%subst(vindta:188:35))
038100091021     C* ......VABTC1
038200091021     C                   IF        VABNT2<>*blanks
038300091021     C                   EVAL      VABTC1='A'
038400091021     C                   ENDIF
038500051028     C* ......VABIND
038600051028     C                   EVAL      VABIND=%trim(%subst(vindta:224:35))
038700050805     C* ......VABLOD
038800051028     C                   EVAL      VABLOD=%trim(%subst(vindta:260:35))
038900050805     C* ......VABCAD
039000051028     C                   EVAL      PiStr=%trim(%subst(vindta:332:10))
039100050805     C                   EXSR      CHKNUM
039200050805     C                   IF        PiInt=*on
039300050805     C                   Z-ADD     PiVal         Num5_0
039400050805     C                   MOVEL(P)  Num5_0        VABCAD
039500050805     C                   ELSE
039600050805     C                   ADD       1             errore
039700050805     C                   EVAL      vinmsg = %trimr(vinmsg)
039800050805     C                             + ' ' + 'VABCAD'
039900050805     C                   ENDIF
040000051028     C* ......VABNZD
040100051028     C                   EVAL      VABNZD=%trim(%subst(vindta:343:3))
040200051028     C                   IF        VABNZD<>*blanks
040300051028     C                   Z-ADD     1             jNAZ
040400051028     C     VABNZD        LOOKUP    SKNAZISO(jNAZ)                         55
040500051028     C                   IF        %found
040600051028     C                   EVAL      VABNZD=SKNAZBAR(jNAZ)
040700051028     C                   ENDIF
040800051028     C                   ENDIF
040900051028     C* ......VABPRD
041000051028     C                   EVAL      VABPRD=%trim(%subst(vindta:296:35))
041100051028     C* Se nn già presente reperisco la provincia dal CAP e dalla località
041200051029     C                   IF        VABCAD <> *blanks AND
041300051029     C                             VABPRD  = *blanks AND
041400051029     C                             VABNZD  = *blanks
041500051028     C                   CLEAR                   TISI95DS
041600051028     C                   EVAL      I95TCN = '3'
041700051028     C                   Z-ADD     datcor        I95DAT
041800051028     C                   EVAL      I95CAP = VABCAD
041900051028     C                   EVAL      I95LOC = VABLOD
042000051028     C                   CALL      'TISI95R'
042100051028     C                   PARM                    TISI95DS
042200051028     C                   EVAL      VABPRD = O95PRV
042300051028     C                   ENDIF
042400051111     C* ......VABCCM
042500051111     C                   IF        %trim(%subst(vindta:455:35))='4011231000000'
042600051111     C                   EVAL      VABCCM = 0433177
042700051117     C                   EVAL      VATCCM = 0433177
042800051111     C                   ENDIF
042900051111     C                   IF        %trim(%subst(vindta:455:35))='4260079420000'
043000051111     C                   EVAL      VABCCM = 0433178
043100051117     C                   EVAL      VATCCM = 0433178
043200051111     C                   ENDIF
043300060825     C                   IF        %trim(%subst(vindta:455:35))='4011231007500'
043400060825     C                   EVAL      VABCCM = 0433550
043500060825     C                   EVAL      VATCCM = 0433550
043600060825     C                   Z-ADD     000           VABCTR
043700060825     C                   ENDIF
043800060920     C                   IF        VABCCM = 9999999
043900060920     C                   EVAL      VABFGS = 999
044000060920     C                   ENDIF
044100060920     C                   IF        VATCCM = 9999999
044200060920     C                   EVAL      VATFGS = 999
044300060920     C                   ENDIF
044400071030     C* ......VABPKB
044500071030     C                   EVAL      PiStr=%trim(%subst(vindta:614:15))
044600071030     C                   EXSR      CHKNUM
044700071030     C                   IF        PiNum=*on
044800071030     C                   Z-ADD(H)  PiVal         VABPKB
044900071030     C                   ELSE
045000111121     C* Forzatura x evitare "porcheria" in conferma in filiale
045100111121     C                   EVAL      VABPKB = 1
045200111121     C                   ADD       1             errore
045300111121     C                   EVAL      vinmsg = %trimr(vinmsg)
045400111121     C                             + ' ' + 'VABPKB'
045500071030     C                   ENDIF
045600051111     C*
045700050805     C                   ENDIF
045800050805     C*
045900050805     C********
046000051028     C* Tipo record 'POS' (barcode, colli, peso e natura merce)
046100050805     C********
046200051028     C                   IF        %trim(%subst(vindta:1:3)) = 'POS'
046300051028     C* ......VATNOT/VATTRC
046400051028     C                   EVAL      VATTRC='E'
046500051111     C***                EVAL      VATNOT=%trim(%subst(vindta:26:10))
046600051111     C                   EVAL      VATNOT=%trim(%subst(vindta:5:20))
046700050805     C* ......VABNCL
046800051028     C                   EVAL      VABNCL = VABNCL + 1
046900071030     C* ......VABPKB
047000071030     C*                  EVAL      PiStr=%trim(%subst(vindta:37:15))
047100071030     C*                  EXSR      CHKNUM
047200071030     C*                  IF        PiNum=*on
047300071030     C*                  ADD(H)    PiVal         VABPKB
047400071030     C*                  ELSE
047500071030     C*                  ADD       1             errore
047600071030     C*                  EVAL      vinmsg = %trimr(vinmsg)
047700071030     C*                            + ' ' + 'VABPKB'
047800071030     C*                  ENDIF
047900051102     C*
048000051102     C* VALORIZZO CAMPI RELATIVI AL "CMR" DEL DETTAGLIO
048100051102     C                   EXSR      VALCMRVAT
048200051102     C*
048300051102     C* SCARICO I BUFFER DEL DETTAGLIO
048400051102     C                   WRITE     EDIVAT00
048500050805     C*
048600050805     C                   ENDIF
048700040910     C*
048800040910     C* Considerazioni sul contenuto di campi precedentemente valorizzati
048900040910     C                   IF        FlgCAS <> '0'
049000040929     C                   IF        VABCBO = '1'
049100040910     C                   EVAL      VABCBO = '4'
049200040910     C                   ELSE
049300040929     C                   EVAL      VABCBO = '6'
049400040910     C                   ENDIF
049500040929     C                   ENDIF
049600040910     C*
049700040910     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049800040910     C                   EXSR      CHKIMPDIV
049900010202     C*
050000000801     C* Ebbene...
050100000801     C                   ADD       1             §CTRMO
050200010201     C                   IF        errore <> *zeros
050300000801     C                   ADD       1             §CTRNO
050400000801     C                   EVAL      vinflg = '2'
050500000801     C                   ELSE
050600010201     C                   ADD       1             §CTROKVB
050700000801     C                   ENDIF
050800000801     C*
050900000801     C                   ENDSR
051000010202     C*----------------------------------------------------*
051100051102     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051200010202     C*----------------------------------------------------*
051300020305     C     PREVAT        BEGSR
051400010202     C*
051500051102     C* Compongo il nome del membro da dare al EDIVATWR
051600010202     C                   eval      parmbr = vlrhdl
051700010202     C                   movel     'M'           parmbr
051800041210     C                   eval      parccm = vlrksc
051900010202     C                   eval      paropz = '1'
052000010202     C* Effettuo la chiamata al CLLE preposto
052100051102     C                   call(e)   'TITVEVTC'
052200010202     C                   parm                    parccm
052300010202     C                   parm                    parmbr
052400010202     C                   parm                    paropz
052500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052600010202     C                   if        %error
052700010202     C                   movel     '1'           chkcall
052800010202     C                   else
052900010202     C                   movel     '0'           chkcall
053000010202     C                   endif
053100010202     C*
053200010202     C                   ENDSR
053300000801     C*----------------------------------------------------*
053400000801     C*  CONTROLLO NUMERICITA' CAMPI
053500000801     C*----------------------------------------------------*
053600000801     C     CHKNUM        BEGSR
053700000801     C*
053800000801     C                   call(e)   'ISNUMERIC'
053900000801     C                   PARM                    PiStr            30
054000041210     C                   PARM      '.'           PiDecChr          1
054100000801     C                   PARM      *ZEROS        PiVal            30 9
054200000801     C                   PARM      '0'           PiInt             1
054300000801     C                   PARM      '0'           PiNum             1
054400000801     C                   IF        %error
054500000801     C                   EVAL      PiInt=*off
054600000801     C                   ENDIF
054700000801     C*
054800000801     C                   ENDSR
054900000801     C***
055000000801
055100011113
055200011113     C*----------------------------------------------------*
055300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055400011113     C*----------------------------------------------------*
055500011113     C     CHKIMPDIV     BEGSR
055600011113     C*
055700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055800011113     C                   Z-ADD     *zeros        wrkDec            9 9
055900011113     C*
056000011113     C* Come prima cosa effettuo considerazioni sulla divisa
056100011113     C                   IF        vabIAS > *zeros
056200011113     C                   IF        vabVAS <> 'EUR'
056300011113     C                   EVAL      vabVAS =  'ITL'
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C                   IF        vabCAS > *zeros
056800011113     C                   IF        vabVCA <> 'EUR'
056900011113     C                   EVAL      vabVCA =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C                   IF        vabVMD > *zeros
057400020305     C                   IF        vabVAD <> 'EUR'
057500011113     C                   EVAL      vabVAD =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058000011113     C                   Z-ADD     vabIAS        wrkDec
058100011113     C                   IF        wrkDec > *zeros
058200011113     C                   IF        vabVAS = 'ITL'
058300011113     C                   EVAL      vabIAS = *zeros
058400011113     C                   ENDIF
058500011113     C                   ENDIF
058600011113     C*
058700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058800011113     C                   Z-ADD     vabCAS        wrkDec
058900011113     C                   IF        wrkDec > *zeros
059000011113     C                   IF        vabVCA = 'ITL'
059100011113     C                   EVAL      vabCAS = *zeros
059200011113     C                   ENDIF
059300011113     C                   ENDIF
059400011113     C*
059500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059600011113     C                   Z-ADD     vabVMD        wrkDec
059700011113     C                   IF        wrkDec > *zeros
059800011113     C                   IF        vabVAD = 'ITL'
059900011113     C                   EVAL      vabVMD = *zeros
060000011113     C                   ENDIF
060100011113     C                   ENDIF
060200011113     C*
060300011113     C                   ENDSR
060400011113     C***
060500011113
060600011113
060700000801
060800000801
060900990920      /TITLE Invio dei dati al punto operativo.
061000010202     C     invio         BEGSR
061100990920     C*
061200051102     C* 1° invio EDIVAT
061300010201     C                   reset                   dscmz
061400010201     C                   move      vlrpoi        cmzdst
061500051102     C                   eval      cmzfld = 'EDIVATWR'
061600010201     C                   eval      cmzmbd = vlrhdl
061700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061800021009     C***                if        prmfir = *blanks
061900051102     C                   eval      cmzfla = 'EDIVAT0F'
062000051102     C                   eval      cmzmba = 'EDIVAT0F'
062100021009     C***                else
062200021009     C***                eval      cmzfla = prmfir
062300021009     C***                eval      cmzmba = prmfir
062400021009     C***                endif
062500010201     C                   eval      cmznrr = *zeros
062600020305     C                   move      §ctrokvt      cmznrr
062700021018     C                   eval      cmzlba = vlrfl1
062800010201     C                   call(e)   'TIS711C'
062900010201     C                   parm                    dscmz
063000010201     C                   parm      *blanks       esito
063100010205     C                   if        %error
063200010205     C                             or cmzerr = '1'
063300010205     C                             or esito  = '1'
063400010205     C                   eval      wrkesito = '3'
063500010205     C                   else
063600010201     C*
063700051102     C* 2° invio EDIVAB
063800010201     C                   reset                   dscmz
063900010201     C                   move      vlrpoi        cmzdst
064000010201     C                   eval      cmzfld = vlrfou
064100010201     C                   eval      cmzmbd = vlrhdl
064200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064300021009     C***                if        prmfir = *blanks
064400051102     C                   eval      cmzfla = 'EDIVAB0F'
064500051102     C                   eval      cmzmba = 'EDIVAB0F'
064600021009     C***                else
064700021009     C***                eval      cmzfla = prmfir
064800021009     C***                eval      cmzmba = prmfir
064900021009     C***                endif
065000010201     C                   eval      cmznrr = *zeros
065100010201     C                   move      §ctrokvb      cmznrr
065200021018     C                   eval      cmzlba = vlrfl1
065300010201     C                   call(e)   'TIS711C'
065400010201     C                   parm                    dscmz
065500010201     C                   parm      *blanks       esito
065600010201     C                   if        %error
065700010201     C                             or cmzerr = '1'
065800010201     C                             or esito  = '1'
065900010201     C                   eval      wrkesito = '3'
066000010201     C                   endif
066100010205     C                   endif
066200990920     C*
066300000613     C                   ENDSR
066400000613     C***
066500070502
066600070502
066700070502
066800070502     C     *pssr         BEGSR
066900070502     C*
067000070502     C                   if        %open(edivabwr)
067100070502     C                   close     edivabwr
067200070502     C                   endif
067300070502     C                   if        %open(edivatwr)
067400070502     C                   close     edivatwr
067500070502     C                   endif
067600070502     C*
067700070502     C* Effettuo la chiamata al CLLE preposto
067800070502     C                   call(e)   'TITVEVTC'
067900070502     C                   parm                    parccm
068000070502     C                   parm                    parmbr
068100070502     C                   parm      '2'           paropz
068200070502     C*
068300070502     C                   eval      wrkesito = '2'
068400070502     C*
068500070502     C                   seton                                        LR
068600070502     C*
068700070502     C                   ENDSR     '*CANCL'
068800070502     C***
068900070502
069000070502
069100990910
069200000613     C     *inzsr        BEGSR
069300990910     C*
069400990910     C     *entry        plist
069500990920     C                   parm                    tivlrds
069600990921     C                   parm      wrkesito      esito
069700000724     C                   parm                    prmlit
069800000710     C                   parm                    prmfir
069900000613     C*
070000000830     C* CALCOLA LA DATA CORRENTE
070100000830     C                   time                    wn14             14 0
070200051102     C                   move      wn14          wn8               8 0          *DATA (8) IN GGMMAA
070300051102     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
070400000830     C                   z-add     wn8           g08dat
070500000830     C                   z-add     *zeros        g08inv
070600000830     C                   movel     '0'           g08err
070700000830     C                   call      'XSRDA8'
070800000830     C                   parm                    wlbda8
070900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071000051115     C                   movel(p)  *all'-'       datoracor        15
071100051117     C                   movel     datcor        datoracor
071200051102     C                   move      oracor        datoracor
071300041210     C*
071400041210     C* Chiave su TABEL00F - parziale
071500041210     C     KEYtabP       KLIST
071600041210     C                   KFLD                    tblKUT
071700041210     C                   KFLD                    tblCOD
071800000830     C*
071900000613     C                   ENDSR
072000000613     C***
