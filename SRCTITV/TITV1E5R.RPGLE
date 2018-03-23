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
002500080310     D wScaricaBuffer  s              1    INZ(*zeros)
002600080401     D wListaBarcode   s                   LIKE(VATNOT) DIM(10000) INZ
002700080401     D idxBarcode      s              5  0 INZ(0)
002800000830
002900000830     D*------------------
003000000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100000830     D*------------------
003200000830     D WLBDA8          DS                  INZ
003300000830     D  G08DAT                 1      8  0
003400000830     D  G08INV                 9     16  0
003500000830     D  G08ERR                17     17
003600000830     D  G08TGI                18     22  0
003700041025     D*------------------
003800041025     D* DS REPERIMENTO NUMERATORE
003900041025     D*------------------
004000041025     D trul33ds      e ds                  inz
004100041025     D*------------------
004200041025     D* DS ARCHITETTURA
004300041025     D*------------------
004400041025     D kpjba         e ds                  inz
004500990908
004600110519     D*------------------
004700110519     D* Costanti
004800110519     D*------------------
004900110519     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000110519     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100110519
005200010201
005300010201
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000613     C*
005800040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
005900070719     C*
006000070719     C* Esegue lancio TISI95R solo x chiusura
006100070719     C                   CLEAR                   TISI95DS
006200070719     C                   EVAL      I95TLA = 'C'
006300070719     C                   CALL      'TISI95R'
006400070719     C                   PARM                    TISI95DS
006500000613     C*
006600010202     C* Effettuo la chiamata al CLLE preposto
006700040506     C                   call(e)   'TITVVTC'
006800010202     C                   parm                    parccm
006900010202     C                   parm                    parmbr
007000010202     C                   parm      '2'           paropz
007100000616     C*
007200010201     C                   seton                                        LR
007300990908
007400000801
007500910830     C*--------------------------------------------------------
007600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
007700910830     C*--------------------------------------------------------
007800040526     C     RWFILE        BEGSR
007900990910     C*
008000990914     C                   if        not %open(tivin00r)
008100990908     C                   open      tivin00r
008200990914     C                   endif
008300021113     C                   if        not %open(fivabwwr)
008400021113     C                   open      fivabwwr
008500990914     C                   endif
008600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
008700020305     C                   exsr      prevat
008800010201     C*
008900010202     C                   if        chkcall = '0'
009000010202     C*
009100021113     C                   if        not %open(fivatwwr)
009200021113     C                   open      fivatwwr
009300010201     C                   endif
009400990910     C*
009500010201     C                   clear                   §CTROKVB          5 0
009600020305     C                   clear                   §CTROKVT          5 0
009700000801     C                   clear                   §CTRMO            5 0
009800000801     C                   clear                   §CTRNO            5 0
009900040910     C*
010000921023     C                   DO        *HIVAL
010100990913     C*
010200990915     C                   READ      tivin00r                               70
010300110519     C*
010400110519     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
010500110519     C     minu:maiu     XLATE     vindta        vindta
010600110519     C*
010700040910     C                   if        vindta > *blanks
010800000613     C                   add       1             rrnum
010900000801     C*
011000000801     C                   if        *in70 = *off
011100000801     C                             and
011200000801     C                             (vinflg = *blanks
011300000801     C                              or vinflg = '0'
011400000801     C                              or vinflg = '2')
011500000801     C*
011600000801     C                   clear                   vinmsg
011700000801     C                   eval      vinflg = '1'
011800040910     C*
011900040910     C* Eseguo routine d traduzione
012000040910     C                   exsr      impvabvat
012100040802     C*
012200010305     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900000905     C*
013000991022     C  N70              ENDdo
013100080310     C*
013200080310     C* SCarico l'ultima testata rimasta in canna
013300080310     C                   WRITE     FIVAB000
013400010202     C*
013500010202     C                   endif
013600990910
013700990910     C* Se non ci sono record con errori ...
013800000710     C                   if        §ctrno = 0
013900990910     C* ... restituisco esito OK.
014000990921     C                   eval      wrkesito = '0'
014100990910     C                   else
014200010201     C                   if        §ctrokvb > 0
014300990921     C                   eval      wrkesito = '1'
014400000710     C                   else
014500000710     C                   eval      wrkesito = '2'
014600990910     C                   endif
014700000710     C                   endif
014800990910     C*
014900990914     C                   if        %open(tivin00r)
015000990908     C                   close     tivin00r
015100990914     C                   endif
015200021113     C                   if        %open(fivabwwr)
015300021113     C                   close     fivabwwr
015400990914     C                   endif
015500021113     C                   if        %open(fivatwwr)
015600021113     C                   close     fivatwwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   if        §ctrokvb > 0
016000000724     C                             and vlrpoi <> *zeros
016100010202     C                   exsr      invio
016200990920     C                   endif
016300990920     C*
016400910830     C                   ENDSR
016500000613     C***
016600990920
016700000801     C*----------------------------------------------------*
016800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
016900000801     C*----------------------------------------------------*
017000010201     C     INZVAR        BEGSR
017100000801     C*
017200040802     C                   Z-ADD     *zeros        Num5_0            5 0
017300040802     C                   MOVEL     '0'           FlgCAS            1
017400000801     C*
017500000801     C                   ENDSR
017600000801     C*----------------------------------------------------*
017700040910     C*  IMPOSTAZIONE CAMPI COSTANTI
017800000801     C*----------------------------------------------------*
017900000801     C     DEFCAM        BEGSR
018000000801     C*
018100021113     C                   CLEAR                   FIVAB000
018200040802     C                   CLEAR                   FIVAT000
018300020619     C* Imposto i valori di default...
018400080310     C                   Z-ADD     0661429       VABCCM
018500080310     C                   Z-ADD     0661429       VATCCM
018600080310     C                   Z-ADD     066           VABLNP
018700080310     C                   Z-ADD     066           VATLNP
018800070531     C                   Z-ADD     000           VABCTR
018900070531     C                   MOVEL     '7Q'          VABCTM
019000040823     C                   MOVEL     '1'           VABCBO
019100020619     C* ... e poi verifico se sono stati passati come parametri
019200020619     C                   IF        vlrppt > *blanks
019300040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
019400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
019500020619     C                   EXSR      CHKNUM
019600020619     C                   IF        PiInt=*on
019700020619     C                   Z-ADD     PiVal         VABCCM
019800020619     C                   Z-ADD     PiVal         VATCCM
019900020619     C                   ENDIF
020000040506     C                   ENDIF
020100040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
020200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
020300020619     C                   EXSR      CHKNUM
020400020619     C                   IF        PiInt=*on
020500020619     C                   Z-ADD     PiVal         VABLNP
020600020619     C                   Z-ADD     PiVal         VATLNP
020700040506     C                   ENDIF
020800020619     C                   ENDIF
020900040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
021000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
021100020619     C                   EXSR      CHKNUM
021200020619     C                   IF        PiInt=*on
021300020619     C                   Z-ADD     PiVal         VABCTR
021400040506     C                   ENDIF
021500020619     C                   ENDIF
021600060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
021700060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
021800060202     C                   ENDIF
021900020619     C                   ENDIF
022000000801     C*
022100000801     C                   ENDSR
022200000801     C*----------------------------------------------------*
022300040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
022400000801     C*----------------------------------------------------*
022500040910     C     IMPVABVAT     BEGSR
022600040910     C*
022700040910     C* Traduzione relativa ai tipi record del file del cliente
022800040910     C*
022900071210     C***
023000080310     C* ...tipo record '1' (testata)
023100080310     C                   IF        %subst(vindta:1:1) = '1'
023200080310     C*
023300080310     C* Ad ogni primo record d dettaglio scarico il buffer della testata
023400080310     C                   IF        wScaricaBuffer = '0'
023500080310     C                   EVAL      wScaricaBuffer = '1'
023600080310     C                   ELSE
023700080310     C                   WRITE     FIVAB000
023800080310     C                   ENDIF
023900080310     C*
024000071210     C* Resetto indicatore d anomalia sul singolo record
024100080310     C                   eval      vinflg = '1'
024200071210     C* ......inizializzazioni iniziali e formati record file Bartolini
024300071210     C                   EXSR      INZVAR
024400071210     C                   EXSR      DEFCAM
024500071210     C*
024600071210     C                   Z-ADD     *zeros        errore            1 0
024700080915     C                   MOVEL     *blanks       wIdCollo          8
024800080310     C                   MOVEL     *blanks       wAnnoBolla        4
024900071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
025000071210     C                   MOVEL     datcor        VABAAS
025100071210     C                   MOVEL     datcor        VATAAS
025200071210     C                   MOVE      datcor        VABMGS
025300071210     C                   MOVE(P)   vlrpoi        VABFGS
025400071210     C                   MOVE(P)   vlrpoi        VATFGS
025500071210     C* ......VABNSP/VATNSP
025600071210     C* NSP => Stacco un numeratore da AZNUM
025700071210     C                   clear                   TRUL33DS
025800071210     C                   eval      I33OPE = *zeros
025900071210     C                   eval      I33CNU = 302
026000071210     C                   eval      I33NUM = 1
026100071210     C                   movel     TRUL33DS      KPJBU
026200071210     C                   call      'TRUL33R'
026300071210     C                   parm                    KPJBA
026400071210     C                   movel     KPJBU         TRUL33DS
026500071210     C                   if        O33ERR = *zeros
026600071210     C                   z-add     O33NRF        VABNSP
026700071210     C                   z-add     O33NRF        VATNSP
026800071210     C                   else
026900071210     C                   Z-ADD     1             errore
027000071210     C                   EVAL      vinmsg = %trimr(vinmsg)
027100071210     C                             + ' ' + 'VABNSP VATNSP'
027200071210     C                   endif
027300060202     C* ......VABRMN
027400080310     C                   EVAL      PiStr=%trim(%subst(vindta:2:5))
027500060202     C                   EXSR      CHKNUM
027600060202     C                   IF        PiInt=*on
027700060202     C                   Z-ADD     PiVal         VABRMN
027800060202     C                   ELSE
027900060202     C                   ADD       1             errore
028000060202     C                   EVAL      vinmsg = %trimr(vinmsg)
028100060202     C                             + ' ' + 'VABRMN'
028200060202     C                   ENDIF
028300060202     C* ......VABPKB
028400080310     C                   EVAL      PiStr=%trim(%subst(vindta:136:7))
028500060202     C                   EXSR      CHKNUM
028600060202     C                   IF        PiNum=*on
028700080310     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
028800070726     C                   Z-ADD(H)  PiVal         VABPKB
028900060202     C                   ELSE
029000060202     C                   ADD       1             errore
029100060202     C                   EVAL      vinmsg = %trimr(vinmsg)
029200060202     C                             + ' ' + 'VABPKB'
029300060202     C                   ENDIF
029400071210     C* ......VABVLB
029500080310     C                   EVAL      PiStr=%trim(%subst(vindta:143:7))
029600080310     C                   EXSR      CHKNUM
029700080310     C                   IF        PiNum=*on
029800080310     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
029900080310     C                   Z-ADD(H)  PiVal         VABVLB
030000080310     C                   ELSE
030100080310     C                   ADD       1             errore
030200080310     C                   EVAL      vinmsg = %trimr(vinmsg)
030300080310     C                             + ' ' + 'VABVLB'
030400080310     C                   ENDIF
030500080310     C* ......campi di wrk
030600080310     C                   EVAL      wAnnoBolla='20'+%trim(%subst(vindta:134:2))
030700071210     C* ......VABRMA
030800080310     C                   EVAL      VABRMA=%trim(%subst(vindta:2:5))
030900060202     C* ......VABRSD
031000080310     C                   EVAL      VABRSD=%trim(%subst(vindta:13:40))
031100060202     C* ......VABIND
031200080310     C                   EVAL      VABIND=%trim(%subst(vindta:53:35))
031300060202     C* ......VABLOD
031400080310     C                   EVAL      VABLOD=%trim(%subst(vindta:93:35))
031500080310     C* ......VABPRD
031600080310     C                   EVAL      VABPRD=%trim(%subst(vindta:128:2))
031700060202     C* ......VABCAD
031800080310     C                   EVAL      PiStr=%trim(%subst(vindta:88:5))
031900060202     C                   EXSR      CHKNUM
032000060202     C                   IF        PiInt=*on
032100060202     C                   Z-ADD     PiVal         Num5_0
032200060202     C                   MOVEL(P)  Num5_0        VABCAD
032300060202     C                   ELSE
032400060202     C                   ADD       1             errore
032500060202     C                   EVAL      vinmsg = %trimr(vinmsg)
032600060202     C                             + ' ' + 'VABCAD'
032700060202     C                   ENDIF
032800060202     C* ......VABPRD
032900070719     C* Reperisco la provincia dal CAP e dalla località
033000070719     C                   IF        VABPRD  = *blanks AND
033100070719     C                             VABCAD <> *blanks
033200070719     C                   CLEAR                   TISI95DS
033300070719     C                   EVAL      I95TCN = '3'
033400070719     C                   Z-ADD     datcor        I95DAT
033500070719     C                   EVAL      I95CAP = VABCAD
033600070719     C                   EVAL      I95LOC = VABLOD
033700070719     C                   CALL      'TISI95R'
033800070719     C                   PARM                    TISI95DS
033900070719     C                   EVAL      VABPRD = O95PRV
034000070719     C                   ENDIF
034100110519     C*
034200110519     C* ......VABCBO
034300110519     C                   SELECT
034400110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
034500110519     C                             'PORTO FRANCO'                  AND
034600110519     C                             %trim(%subst(vindta:175:16)) = *blanks
034700110519     C                   EVAL      VABCBO = '1'
034800110519     C*
034900110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
035000110519     C                             'PORTO GRATIS C/R'              AND
035100110519     C                             %trim(%subst(vindta:175:16)) = *blanks
035200110519     C                   EVAL      VABCBO = '1'
035300110519     C*
035400110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
035500110519     C                             'PORTO ASSEGNATO'               AND
035600110519     C                             %trim(%subst(vindta:175:16)) = *blanks
035700110519     C                   EVAL      VABCBO = '2'
035800110519     C*
035900110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
036000110519     C                             'PORTO FRANCO'                  AND
036100110519     C                             %trim(%subst(vindta:175:16)) = 'CONTRASSEGNO'
036200110519     C                   EVAL      VABCBO = '4'
036300110519     C*
036400110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
036500110519     C                             'PORTO GRATIS C/R'              AND
036600110519     C                             %trim(%subst(vindta:175:16)) = 'CONTRASSEGNO'
036700110519     C                   EVAL      VABCBO = '4'
036800110519     C*
036900110519     C                   WHEN      %trim(%subst(vindta:155:20)) =
037000110519     C                             'PORTO ASSEGNATO'               AND
037100110519     C                             %trim(%subst(vindta:175:16)) = 'CONTRASSEGNO'
037200110519     C                   EVAL      VABCBO = '6'
037300110519     C*
037400110519     C                   ENDSL
037500060202     C*
037600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
037700060202     C                   IF        FlgCAS <> '0'
037800060202     C                   IF        VABCBO = '1'
037900060202     C                   EVAL      VABCBO = '4'
038000110519     C                   ENDIF
038100110519     C                   IF        VABCBO = '2'
038200060202     C                   EVAL      VABCBO = '6'
038300060202     C                   ENDIF
038400060202     C                   ENDIF
038500060202     C*
038600060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038700060202     C                   EXSR      CHKIMPDIV
038800040910     C*
038900040910     C                   ENDIF
039000080310     C***
039100080310     C* ...tipo record '2' (eventuale destinazione alternativa merce)
039200080310     C                   IF        %subst(vindta:1:1) = '2'
039300080310     C* Resetto indicatore d anomalia sul singolo record
039400080310     C                   eval      vinflg = '1'
039500080310     C* ......VABRSD
039600080310     C                   EVAL      VABRSD=%trim(%subst(vindta:7:40))
039700080310     C* ......VABIND
039800080310     C                   EVAL      VABIND=%trim(%subst(vindta:47:35))
039900080310     C* ......VABLOD
040000080310     C                   EVAL      VABLOD=%trim(%subst(vindta:87:35))
040100080310     C* ......VABPRD
040200080310     C                   EVAL      VABPRD=%trim(%subst(vindta:122:2))
040300080310     C* ......VABCAD
040400080310     C                   EVAL      PiStr=%trim(%subst(vindta:82:5))
040500080310     C                   EXSR      CHKNUM
040600080310     C                   IF        PiInt=*on
040700080310     C                   Z-ADD     PiVal         Num5_0
040800080310     C                   MOVEL(P)  Num5_0        VABCAD
040900080310     C                   ELSE
041000080310     C                   ADD       1             errore
041100080310     C                   EVAL      vinmsg = %trimr(vinmsg)
041200080310     C                             + ' ' + 'VABCAD'
041300080310     C                   ENDIF
041400080310     C* ......VABPRD
041500080310     C* Reperisco la provincia dal CAP e dalla località
041600080310     C                   IF        VABPRD  = *blanks AND
041700080310     C                             VABCAD <> *blanks
041800080310     C                   CLEAR                   TISI95DS
041900080310     C                   EVAL      I95TCN = '3'
042000080310     C                   Z-ADD     datcor        I95DAT
042100080310     C                   EVAL      I95CAP = VABCAD
042200080310     C                   EVAL      I95LOC = VABLOD
042300080310     C                   CALL      'TISI95R'
042400080310     C                   PARM                    TISI95DS
042500080310     C                   EVAL      VABPRD = O95PRV
042600080310     C                   ENDIF
042700080310     C*
042800080310     C                   ENDIF
042900040910     C***
043000080310     C* ...tipo record '3' (dettaglio colli)
043100080310     C                   IF        %subst(vindta:1:1) = '3'
043200080310     C*
043300070719     C* Resetto indicatore d anomalia sul singolo record
043400070719     C                   eval      vinflg = '1'
043500080310     C* ......VATNOT
043600081010     C                   IF        %trim(%subst(vindta:134:6)) <> wIdCollo
043700080310     C*
043800081010     C                   EVAL      wIdCollo=%trim(%subst(vindta:134:6))
043900081010     C*
044000080310     C* Applico determinate considerazioni x comporre il relativo barcode su etichetta
044100080916     C*                  IF        %subst(wIdCollo:1:2) = 'O2'
044200080916     C*                  EVAL      VATNOT = 'PRO' + wAnnoBolla + '0' +
044300080916     C*                                     %trim(%subst(wIdCollo:3))
044400080916     C*                  ENDIF
044500080310     C*
044600080916     C*                  IF        %subst(wIdCollo:1:2) = 'X2'
044700080916     C*                  EVAL      VATNOT = 'BOX' + wAnnoBolla + '0' +
044800080916     C*                                     %trim(%subst(wIdCollo:3))
044900080916     C*                  ENDIF
045000080310     C*
045100080916     C*                  IF        %subst(wIdCollo:1:2) = 'U2'
045200080916     C*                  EVAL      VATNOT = 'SFU' + wAnnoBolla + '0' +
045300080916     C*                                     %trim(%subst(wIdCollo:3))
045400080916     C*                  ENDIF
045500080310     C*
045600080916     C*                  IF        %subst(wIdCollo:1:2) = '08'
045700080916     C*                  EVAL      VATNOT = wAnnoBolla + '001' + '0' +
045800080916     C*                                     %trim(%subst(wIdCollo:3))
045900080916     C*                  ENDIF
046000080916     C*
046100081010     C*                  IF        %subst(wIdCollo:1:2) = '08'
046200081010     C*                  EVAL      VATNOT = '20' + %trim(wIdCollo)
046300081010     C*                  ENDIF
046400081010     C*
046500081010     C                   EVAL      VATNOT = '008008392' + wAnnoBolla +
046600081010     C                                      %trim(wIdCollo)
046700081010     C* Eseguo calcolo check-digit
046800081010     C                   CALL      'TRUL35R1'
046900081010     C                   PARM      VATNOT        InBarcode        35
047000081010     C                   PARM      'EANP'        InTip             4
047100081010     C                   PARM      *blanks       InDsply           1
047200081010     C                   PARM      *blanks       OutBarcode       36
047300081010     C*
047400081010     C                   EVAL      VATNOT = OutBarcode
047500080401     C*
047600080401     C* Verifico se barcode corrente già considerato
047700080401     C     VATNOT        LOOKUP    wListaBarcode                          80
047800080401     C                   IF        not %found
047900080401     C                   ADD       1             idxBarcode
048000080401     C                   EVAL      wListaBarcode(idxBarcode)=VATNOT
048100080401     C*
048200080401     C* Incremento il numero dei colli
048300080424     C* - Forzatura x gli "sfusi" => nn acquisire
048400080424     C                   IF        %subst(wIdCollo:1:2) <> 'U2'
048500080401     C                   ADD       1             VABNCL
048600080424     C                   ENDIF
048700080310     C*
048800080310     C* Quindi scarico il buffer....
048900040910     C                   EVAL      VATTRC = 'E'
049000080401     C*
049100080424     C* X ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
049200060202     C                   IF        VATNOT <> *blanks
049300080424     C* - Forzatura x gli "sfusi" => nn acquisire
049400080424     C                   IF        %subst(wIdCollo:1:2) <> 'U2'
049500040910     C                   WRITE     FIVAT000
049600080424     C                   ENDIF
049700060202     C                   ENDIF
049800080310     C                   ENDIF
049900040910     C*
050000080401     C                   ENDIF
050100080401     C*
050200040910     C                   ENDIF
050300010202     C*
050400000801     C* Ebbene...
050500000801     C                   ADD       1             §CTRMO
050600010201     C                   IF        errore <> *zeros
050700000801     C                   ADD       1             §CTRNO
050800000801     C                   EVAL      vinflg = '2'
050900000801     C                   ELSE
051000010201     C                   ADD       1             §CTROKVB
051100000801     C                   ENDIF
051200000801     C*
051300000801     C                   ENDSR
051400010202     C*----------------------------------------------------*
051500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051600010202     C*----------------------------------------------------*
051700020305     C     PREVAT        BEGSR
051800010202     C*
051900021113     C* Compongo il nome del membro da dare al FIVATWWR
052000010202     C                   eval      parmbr = vlrhdl
052100010202     C                   movel     'M'           parmbr
052200060113     C                   eval      parccm = vlrksc
052300010202     C                   eval      paropz = '1'
052400010202     C* Effettuo la chiamata al CLLE preposto
052500040506     C                   call(e)   'TITVVTC'
052600010202     C                   parm                    parccm
052700010202     C                   parm                    parmbr
052800010202     C                   parm                    paropz
052900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053000010202     C                   if        %error
053100010202     C                   movel     '1'           chkcall
053200010202     C                   else
053300010202     C                   movel     '0'           chkcall
053400010202     C                   endif
053500010202     C*
053600010202     C                   ENDSR
053700000801     C*----------------------------------------------------*
053800000801     C*  CONTROLLO NUMERICITA' CAMPI
053900000801     C*----------------------------------------------------*
054000000801     C     CHKNUM        BEGSR
054100000801     C*
054200000801     C                   call(e)   'ISNUMERIC'
054300000801     C                   PARM                    PiStr            30
054400040714     C                   PARM      ','           PiDecChr          1
054500000801     C                   PARM      *ZEROS        PiVal            30 9
054600000801     C                   PARM      '0'           PiInt             1
054700000801     C                   PARM      '0'           PiNum             1
054800000801     C                   IF        %error
054900000801     C                   EVAL      PiInt=*off
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300000801     C***
055400000801
055500011113
055600011113     C*----------------------------------------------------*
055700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055800011113     C*----------------------------------------------------*
055900011113     C     CHKIMPDIV     BEGSR
056000011113     C*
056100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056200011113     C                   Z-ADD     *zeros        wrkDec            9 9
056300011113     C*
056400011113     C* Come prima cosa effettuo considerazioni sulla divisa
056500011113     C                   IF        vabIAS > *zeros
056600011113     C                   IF        vabVAS <> 'EUR'
056700011113     C                   EVAL      vabVAS =  'ITL'
056800011113     C                   ENDIF
056900011113     C                   ENDIF
057000011113     C*
057100011113     C                   IF        vabCAS > *zeros
057200011113     C                   IF        vabVCA <> 'EUR'
057300011113     C                   EVAL      vabVCA =  'ITL'
057400011113     C                   ENDIF
057500011113     C                   ENDIF
057600011113     C*
057700011113     C                   IF        vabVMD > *zeros
057800020305     C                   IF        vabVAD <> 'EUR'
057900011113     C                   EVAL      vabVAD =  'ITL'
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058400011113     C                   Z-ADD     vabIAS        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVAS = 'ITL'
058700011113     C                   EVAL      vabIAS = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059200011113     C                   Z-ADD     vabCAS        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVCA = 'ITL'
059500011113     C                   EVAL      vabCAS = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060000011113     C                   Z-ADD     vabVMD        wrkDec
060100011113     C                   IF        wrkDec > *zeros
060200011113     C                   IF        vabVAD = 'ITL'
060300011113     C                   EVAL      vabVMD = *zeros
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C                   ENDSR
060800011113     C***
060900011113
061000011113
061100000801
061200000801
061300990920      /TITLE Invio dei dati al punto operativo.
061400010202     C     invio         BEGSR
061500990920     C*
061600021113     C* 1° invio FIVAT
061700010201     C                   reset                   dscmz
061800010201     C                   move      vlrpoi        cmzdst
061900021113     C                   eval      cmzfld = 'FIVATWWR'
062000010201     C                   eval      cmzmbd = vlrhdl
062100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062200021009     C***                if        prmfir = *blanks
062300021113     C                   eval      cmzfla = 'FIVAT00F'
062400021113     C                   eval      cmzmba = 'FIVAT00F'
062500021009     C***                else
062600021009     C***                eval      cmzfla = prmfir
062700021009     C***                eval      cmzmba = prmfir
062800021009     C***                endif
062900010201     C                   eval      cmznrr = *zeros
063000020305     C                   move      §ctrokvt      cmznrr
063100021018     C                   eval      cmzlba = vlrfl1
063200010201     C                   call(e)   'TIS711C'
063300010201     C                   parm                    dscmz
063400010201     C                   parm      *blanks       esito
063500010205     C                   if        %error
063600010205     C                             or cmzerr = '1'
063700010205     C                             or esito  = '1'
063800010205     C                   eval      wrkesito = '3'
063900010205     C                   else
064000010201     C*
064100021113     C* 2° invio FIVAB
064200010201     C                   reset                   dscmz
064300010201     C                   move      vlrpoi        cmzdst
064400010201     C                   eval      cmzfld = vlrfou
064500010201     C                   eval      cmzmbd = vlrhdl
064600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064700021009     C***                if        prmfir = *blanks
064800021113     C                   eval      cmzfla = 'FIVAB00F'
064900021113     C                   eval      cmzmba = 'FIVAB00F'
065000021009     C***                else
065100021009     C***                eval      cmzfla = prmfir
065200021009     C***                eval      cmzmba = prmfir
065300021009     C***                endif
065400010201     C                   eval      cmznrr = *zeros
065500010201     C                   move      §ctrokvb      cmznrr
065600021018     C                   eval      cmzlba = vlrfl1
065700010201     C                   call(e)   'TIS711C'
065800010201     C                   parm                    dscmz
065900010201     C                   parm      *blanks       esito
066000010201     C                   if        %error
066100010201     C                             or cmzerr = '1'
066200010201     C                             or esito  = '1'
066300010201     C                   eval      wrkesito = '3'
066400010201     C                   endif
066500010205     C                   endif
066600990920     C*
066700000613     C                   ENDSR
066800000613     C***
066900070411
067000070411     C     *pssr         BEGSR
067100070411     C*
067200070411     C                   if        %open(tivin00r)
067300070411     C                   close     tivin00r
067400070411     C                   endif
067500070411     C                   if        %open(fivabwwr)
067600070411     C                   close     fivabwwr
067700070411     C                   endif
067800070411     C                   if        %open(fivatwwr)
067900070411     C                   close     fivatwwr
068000070411     C                   endif
068100070411     C*
068200070411     C* Effettuo la chiamata al CLLE preposto
068300070411     C                   call(e)   'TITVVTC'
068400070411     C                   parm                    parccm
068500070411     C                   parm                    parmbr
068600070411     C                   parm      '2'           paropz
068700070411     C*
068800070411     C                   eval      wrkesito = '2'
068900070411     C*
069000070411     C                   seton                                        LR
069100070411     C*
069200070411     C                   ENDSR     '*CANCL'
069300070411     C***
069400070411
069500990910
069600000613     C     *inzsr        BEGSR
069700990910     C*
069800990910     C     *entry        plist
069900990920     C                   parm                    tivlrds
070000990921     C                   parm      wrkesito      esito
070100000724     C                   parm                    prmlit
070200000710     C                   parm                    prmfir
070300000613     C*
070400000830     C* CALCOLA LA DATA CORRENTE
070500000830     C                   time                    wn14             14 0
070600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
070700000830     C                   z-add     wn8           g08dat
070800000830     C                   z-add     *zeros        g08inv
070900000830     C                   movel     '0'           g08err
071000000830     C                   call      'XSRDA8'
071100000830     C                   parm                    wlbda8
071200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
071300000830     C*
071400000613     C                   ENDSR
071500000613     C***
