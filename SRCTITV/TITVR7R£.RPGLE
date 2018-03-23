000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800060307     Ftitvr7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100060307     Ftitvr7ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000060307     D depspe          s             15    INZ(*blanks)
004100060307     D curspe          s             15    INZ(*blanks)
004200060307     D cntspe          s              7  0 INZ(*zeros)
004300000830
004400000830     D*------------------
004500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600000830     D*------------------
004700000830     D WLBDA8          DS                  INZ
004800000830     D  G08DAT                 1      8  0
004900000830     D  G08INV                 9     16  0
005000000830     D  G08ERR                17     17
005100000830     D  G08TGI                18     22  0
005200000830     D*
005300990908
005400060307     D*------------------
005500060307     D* DS REPERIMENTO NUMERATORE
005600060307     D*------------------
005700060307     D trul33ds      e ds                  inz
005800060307     D*------------------
005900060307     D* DS ARCHITETTURA
006000060307     D*------------------
006100060307     D kpjba         e ds                  inz
006200060307     D*------------------
006300010201
006400010201
006500990915     C                   time                    wrkdata
006600990915     C                   time                    wrkora
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000724     C*
007100000724     C* SE OCCORRE SPEDIRE IN FILIALE
007200000724     C                   if        vlrpoi <> *zeros
007300000724     C*
007400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600000724     C     vlrpoi        chain     azorg01l
007700000724     C                   if        %found
007800000616     C                   movel(p)  CMD(1)        depcmd
007900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000724     C*
008700000724     C                   endif
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000616     C*
010000060307     C                   if        not %open(titvr7ps)
010100060307     C                   open      titvr7ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400000613     C*
010500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900040506     C                   call(e)   'TITVVTC'
011000010202     C                   parm                    parccm
011100010202     C                   parm                    parmbr
011200010202     C                   parm      '2'           paropz
011300050201     C*
011400050201     C* Effettuo lancio TISI95 solo x chiusura
011500050201     C                   CLEAR                   TISI95DS
011600050201     C                   EVAL      I95TLA = 'C'
011700050201     C                   CALL      'TISI95R'
011800050201     C                   PARM                    TISI95DS
011900010202     C*
012000060307     C                   if        %open(titvr7ps)
012100000616     C                   except    findett
012200060307     C                   close     titvr7ps
012300000613     C                   endif
012400000616     C*
012500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012600011113     C                   Z-ADD     150           LENGH            15 5
012700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000616     C                   PARM                    CMD(3)
012900000616     C                   PARM                    LENGH
013000000616     C*
013100000801     C
013200010201     C                   seton                                        LR
013300000613
013400000613
013500000613     C*--------------------------------------------------------
013600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013700000613     C*--------------------------------------------------------
013800000621     C     STPR          BEGSR
013900000613     C*
014000060307     C                   if        not %open(titvr7p)
014100060307     C                   open      titvr7p
014200990915     C                   endif
014300990915     C*
014400990915     C                   except    riepilogo
014500990915     C*
014600060307     C                   if        %open(titvr7p)
014700060307     C                   close     titvr7p
014800990914     C                   endif
014900990910     C*
015000000613     C                   ENDSR
015100000613     C***
015200990908
015300000801
015400910830     C*--------------------------------------------------------
015500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015600910830     C*--------------------------------------------------------
015700040526     C     RWFILE        BEGSR
015800990910     C*
015900990914     C                   if        not %open(tivin00r)
016000990908     C                   open      tivin00r
016100990914     C                   endif
016200021113     C                   if        not %open(fivabwwr)
016300021113     C                   open      fivabwwr
016400990914     C                   endif
016500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016600020305     C                   exsr      prevat
016700010201     C*
016800010202     C                   if        chkcall = '0'
016900010202     C*
017000021113     C                   if        not %open(fivatwwr)
017100021113     C                   open      fivatwwr
017200010201     C                   endif
017300990910     C*
017400010201     C                   clear                   §CTROKVB          5 0
017500020305     C                   clear                   §CTROKVT          5 0
017600000801     C                   clear                   §CTRMO            5 0
017700000801     C                   clear                   §CTRNO            5 0
017800990910     C*
017900921023     C                   DO        *HIVAL
018000990913     C*
018100990915     C                   READ      tivin00r                               70
018200050627     C                   if        vindta > *blanks
018300000613     C                   add       1             rrnum
018400000801     C*
018500000801     C                   if        *in70 = *off
018600000801     C                             and
018700000801     C                             (vinflg = *blanks
018800000801     C                              or vinflg = '0'
018900000801     C                              or vinflg = '2')
019000000801     C*
019100000801     C                   clear                   vinmsg
019200000801     C                   eval      vinflg = '1'
019300010305     C*
019400050628     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019500060307     C                   eval      PiStr=%trim(%subst(vindta:221:15))
019600050628     C                   movel(p)  PiStr         curspe
019700050628     C                   add       1             cntspe
019800050628     C*
019900050628     C                   if        cntspe = 1
020000050628     C* A rottura inizializzo campi di totalizzazione
020100050628     C                   clear                   fivab000
020200050628     C*
020300050628     C                   exsr      impvab                                       => carico VAB
020400050628     C                   eval      depspe = curspe
020500050628     C                   else
020600050628     C                   if        curspe <> depspe
020700040802     C                   exsr      wrivab
020800050628     C* A rottura inizializzo campi di totalizzazione
020900050628     C                   clear                   fivab000
021000050628     C*
021100050628     C                   exsr      impvab                                       => carico VAB
021200050628     C                   eval      depspe = curspe
021300050628     C                   else
021400050628     C                   exsr      impvab                                       => carico VAB
021500050628     C                   endif
021600050628     C                   endif
021700000905     C*
021800000905     C                   else
021900000905     C                   eval      vinflg = '1'
022000050628     C                   endif
022100000905     C                   endif
022200000905     C*
022300000905     C  N70              update    tivin000
022400000905     C*
022500991022     C  N70              ENDdo
022600040823     C*
022700040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
022800040823     C                   exsr      wrivab
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700010201     C                   if        §ctrokvb > 0
023800990921     C                   eval      wrkesito = '1'
023900000710     C                   else
024000000710     C                   eval      wrkesito = '2'
024100990910     C                   endif
024200000710     C                   endif
024300990910     C*
024400990914     C                   if        %open(tivin00r)
024500990908     C                   close     tivin00r
024600990914     C                   endif
024700021113     C                   if        %open(fivabwwr)
024800021113     C                   close     fivabwwr
024900990914     C                   endif
025000021113     C                   if        %open(fivatwwr)
025100021113     C                   close     fivatwwr
025200010201     C                   endif
025300990910     C*
025400010201     C                   if        §ctrokvb > 0
025500000724     C                             and vlrpoi <> *zeros
025600010202     C                   exsr      invio
025700990920     C                   endif
025800990920     C*
025900910830     C                   ENDSR
026000000613     C***
026100010305
026200010305     C*----------------------------------------------------*
026300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026400010305     C*----------------------------------------------------*
026500020305     C     WRIVAB        BEGSR
026600010305     C*
026700060225     C* Quindi scarico il buffer del file d testata
026800021113     C                   write     fivab000                                     => scarico il VAB
026900010305     C*
027000010305     C                   ENDSR
027100990920
027200000801     C*----------------------------------------------------*
027300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027400000801     C*----------------------------------------------------*
027500010201     C     INZVAR        BEGSR
027600000801     C*
027700040802     C                   Z-ADD     *zeros        Num5_0            5 0
027800040802     C                   MOVEL     '0'           FlgCAS            1
027900060509     C                   MOVEL     *blanks       FlgDSK            1
028000000801     C*
028100000801     C                   ENDSR
028200000801     C*----------------------------------------------------*
028300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028400000801     C*----------------------------------------------------*
028500000801     C     DEFCAM        BEGSR
028600000801     C*
028700020619     C* Imposto i valori di default...
028800060307     C                   Z-ADD     0502978       VABCCM
028900060307     C                   Z-ADD     0502978       VATCCM
029000060307     C                   Z-ADD     050           VABLNP
029100060307     C                   Z-ADD     050           VATLNP
029200040714     C                   Z-ADD     000           VABCTR
029300040823     C                   MOVEL     '1'           VABCBO
029400020619     C* ... e poi verifico se sono stati passati come parametri
029500020619     C                   IF        vlrppt > *blanks
029600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABCCM
030100020619     C                   Z-ADD     PiVal         VATCCM
030200020619     C                   ENDIF
030300040506     C                   ENDIF
030400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABLNP
030900020619     C                   Z-ADD     PiVal         VATLNP
031000040506     C                   ENDIF
031100020619     C                   ENDIF
031200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
031300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031400020619     C                   EXSR      CHKNUM
031500020619     C                   IF        PiInt=*on
031600020619     C                   Z-ADD     PiVal         VABCTR
031700040506     C                   ENDIF
031800020619     C                   ENDIF
031900060509     C                   IF        %subst(vlrppt:14:1) <> *blanks
032000060509     C                   EVAL      FlgDSK = %subst(vlrppt:14:1)
032100060509     C                   ENDIF
032200020619     C                   ENDIF
032300000801     C*
032400000801     C                   ENDSR
032500000801     C*----------------------------------------------------*
032600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032700000801     C*----------------------------------------------------*
032800040823     C     IMPVAB        BEGSR
032900040823     C*
033000020305     C                   EXSR      INZVAR
033100020305     C                   EXSR      DEFCAM
033200010305     C*
033300000801     C                   Z-ADD     *zeros        errore            1 0
033400000830     C                   MOVEL     datcor        VABAAS
033500020305     C                   MOVEL     datcor        VATAAS
033600040526     C                   MOVE      datcor        VABMGS
033700040823     C                   MOVE(P)   vlrpoi        VABFGS
033800040823     C                   MOVE(P)   vlrpoi        VATFGS
033900050628     C*
034000060307     C                   EVAL      VABRSD=%trim(%subst(vindta:20:35))
034100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034300020117     C* ==
034400060307     C                   EVAL      VABIND=%trim(%subst(vindta:55:35))
034500061120     C                   EVAL      VABCAD=%trim(%subst(vindta:90:9))
034600060307     C                   EVAL      VABLOD=%trim(%subst(vindta:99:25))
034700060307     C                   EVAL      VABPRD=%trim(%subst(vindta:124:2))
034800060307     C                   EVAL      VABNZD=%trim(%subst(vindta:126:3))
034900060307     C                   EVAL      VABRMA=%trim(%subst(vindta:221:15))
035000060307     C                   EVAL      VABNAS=%trim(%subst(vindta:238:35))
035100060307     C                   EVAL      VABNOT=%trim(%subst(vindta:154:35))
035200060307     C                   EVAL      VABNT2=%trim(%subst(vindta:189:20))
035300060307     C*
035400060307     C* Determino il tipo porto della spedizione corrente
035500081008     C                   IF        %subst(vindta:406:1) = 'M'
035600060307     C                   EVAL      VABCBO = '1'
035700060307     C                   ENDIF
035800081008     C                   IF        %subst(vindta:406:1) = 'D'
035900060307     C                   EVAL      VABCBO = '2'
036000060307     C                   ENDIF
036100060704     C*
036200060704     C* Determino il tipo d traffico il questione
036300060801     C                   IF        %subst(vindta:319:3) = 'DSI'
036400060704     C                   EVAL      VABCTR = 000
036500060704     C                   ENDIF
036600060704     C                   IF        %subst(vindta:319:3) = 'BMC'
036700060914     C                   EVAL      VABCCM = 0503181
036800060704     C                   EVAL      VABCTR = 399
036900060704     C                   ENDIF
037000061016     C                   IF        %subst(vindta:319:3) = 'EEB'
037100061016     C                   EVAL      VABCCM = 0503181
037200061018     C                   EVAL      VABCTR = 091
037300061016     C                   ENDIF
037400050628     C* CAP
037500061120     C*                  EVAL      PiStr=%trim(%subst(vindta:90:9))
037600061120     C*                  EXSR      CHKNUM
037700061120     C*                  IF        PiInt=*on
037800061120     C*                  Z-ADD     PiVal         Num5_0
037900061120     C*                  MOVEL(P)  Num5_0        VABCAD
038000061120     C*                  ELSE
038100061120     C*                  ADD       1             errore
038200061120     C*                  EVAL      vinmsg = %trimr(vinmsg)
038300061120     C*                            + ' ' + 'VABCAD'
038400061120     C*                  ENDIF
038500040506     C* Reperisco la provincia dal CAP e dalla località
038600040526     C                   IF        VABCAD <> *blanks AND
038700040526     C                             VABPRD  = *blanks
038800040506     C                   CLEAR                   TISI95DS
038900040506     C                   EVAL      I95TCN = '3'
039000040506     C                   Z-ADD     datcor        I95DAT
039100040506     C                   EVAL      I95CAP = VABCAD
039200040506     C                   EVAL      I95LOC = VABLOD
039300050627     C                   EVAL      I95NAR = VABNZD
039400040506     C                   CALL      'TISI95R'
039500040506     C                   PARM                    TISI95DS
039600040506     C                   EVAL      VABPRD = O95PRV
039700040506     C                   ENDIF
039800060307     C* NCL
039900060307     C                   EVAL      PiStr=%trim(%subst(vindta:209:3))
040000060307     C                   EXSR      CHKNUM
040100060307     C                   IF        PiInt=*on
040200060307     C                   ADD       PiVal         VABNCL
040300060307     C                   ELSE
040400060307     C                   ADD       1             errore
040500060307     C                   EVAL      vinmsg = %trimr(vinmsg)
040600060307     C                             + ' ' + 'VABNCL'
040700060307     C                   ENDIF
040800040506     C* PKB
040900060307     C                   EVAL      PiStr=%trim(%subst(vindta:212:7))
041000010201     C                   EXSR      CHKNUM
041100010201     C                   IF        PiNum=*on
041200050628     C                   EVAL(h)   Pival = Pival/100                            * gestisco 2 decim.
041300060307     C                   ADD       PiVal         VABPKB
041400010201     C                   ELSE
041500010201     C                   ADD       1             errore
041600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
041700010201     C                             + ' ' + 'VABPKB'
041800010201     C                   ENDIF
041900070117     C* RMN
042000070117     C***                EVAL      PiStr=%trim(%subst(vindta:221+3:15-3))
042100070117     C                   EVAL      PiStr=%trim(%subst(vindta:331:30))
042200050928     C                   EXSR      CHKNUM
042300050928     C                   IF        PiInt=*on
042400050928     C                   Z-ADD     PiVal         VABRMN
042500050928     C                   ELSE
042600050928     C                   ADD       1             errore
042700050928     C                   EVAL      vinmsg = %trimr(vinmsg)
042800060307     C                             + ' ' + 'VABRMN'
042900050928     C                   ENDIF
043000060307     C* NSP => Stacco un numeratore da AZNUM
043100060307     C                   clear                   TRUL33DS
043200060307     C                   eval      I33OPE = *zeros
043300060307     C                   eval      I33CNU = 302
043400060307     C                   eval      I33NUM = 1
043500060307     C                   movel     TRUL33DS      KPJBU
043600060307     C                   call      'TRUL33R'
043700060307     C                   parm                    KPJBA
043800060307     C                   movel     KPJBU         TRUL33DS
043900060307     C                   if        O33ERR = *zeros
044000060307     C                   z-add     O33NRF        VABNSP
044100060307     C                   z-add     O33NRF        VATNSP
044200060307     C                   else
044300060307     C                   Z-ADD     1             errore
044400060307     C                   EVAL      vinmsg = %trimr(vinmsg)
044500060307     C                             + ' ' + 'VABNSP VATNSP'
044600060307     C                   endif
044700060307     C* CAS
044800060307     C                   IF        %subst(vindta:273:13) <> *zeros
044900060307     C                   EVAL      FlgCAS = '1'
045000060307     C                   EVAL      VABVCA=%trim(%subst(vindta:286:3))
045100060307     C                   ENDIF
045200060307     C                   EVAL      PiStr=%trim(%subst(vindta:273:13))
045300060307     C                   EXSR      CHKNUM
045400060307     C                   IF        PiNum=*on
045500060307     C                   EVAL(H)   VABCAS=PiVal/100                             * gestisco 2 dec.
045600060307     C                   ELSE
045700060307     C                   ADD       1             errore
045800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
045900060307     C                             + ' ' + 'VABCAS'
046000060307     C                   ENDIF
046100060307     C* VMD
046200060307     C                   EVAL      PiStr=%trim(%subst(vindta:290:13))
046300060307     C                   EXSR      CHKNUM
046400060307     C                   IF        PiNum=*on
046500060307     C                   EVAL(H)   VABVMD=PiVal/100                             * gestisco 2 dec.
046600060307     C                   ELSE
046700060307     C                   ADD       1             errore
046800060307     C                   EVAL      vinmsg = %trimr(vinmsg)
046900060307     C                             + ' ' + 'VABVMD'
047000060307     C                   ENDIF
047100060307     C* VAD
047200060307     C                   EVAL      VABVAD=%trim(%subst(vindta:286:3))
047300060307     C* IAS
047400060307     C                   EVAL      PiStr=%trim(%subst(vindta:303:13))
047500060307     C                   EXSR      CHKNUM
047600060307     C                   IF        PiNum=*on
047700060307     C                   EVAL(H)   VABIAS=PiVal/100                             * gestisco 2 dec.
047800060307     C                   ELSE
047900060307     C                   ADD       1             errore
048000060307     C                   EVAL      vinmsg = %trimr(vinmsg)
048100060307     C                             + ' ' + 'VABIAS'
048200060307     C                   ENDIF
048300060307     C* VAS
048400060307     C                   EVAL      VABVAS=%trim(%subst(vindta:286:3))
048500060307     C*
048600060307     C* Calcolo il volume del'intera spedizione
048700060307     C                   EVAL      PiStr=%trim(%subst(vindta:322:3))
048800060307     C                   EXSR      CHKNUM
048900060307     C                   IF        PiInt=*on
049000060307     C                   Z-ADD(H)  PiVal         wAlt              7 3
049100060308     C                   EVAL      wAlt  = wAlt/100                             * da cm a metri
049200060307     C                   EVAL      PiStr=%trim(%subst(vindta:325:3))
049300060307     C                   EXSR      CHKNUM
049400060307     C                   IF        PiInt=*on
049500060307     C                   Z-ADD(H)  PiVal         wLarg             7 3
049600060308     C                   EVAL      wLarg = wLarg/100                            * da cm a metri
049700060307     C                   EVAL      PiStr=%trim(%subst(vindta:328:3))
049800060307     C                   EXSR      CHKNUM
049900060307     C                   IF        PiInt=*on
050000060307     C                   Z-ADD(H)  PiVal         wProf             7 3
050100060307     C                   EVAL      wProf = wProf/100                            * da cm a metri
050200060307     C                   ENDIF
050300060307     C                   ENDIF
050400060307     C                   ENDIF
050500060331     C***                EVAL      VABVLB=VABVLB+(wAlt*wLarg*wProf*VABNCL)
050600060307     C*
050700060509     C* Se richiesto nei parametri scrivo estensione dettaglio bolle - tipo record "E" => "CHI SONO"
050800060509     C                   if        FlgDSK = 'C'
050900060509     C                   MOVEL     '7Q'          VABCTM
051000060307     C                   exsr      exevate
051100060509     C                   endif
051200060307     C* ...scrivo anche estensione dettaglio bolle - tipo record "A" => REFERENTE DEST.
051300060307     C                   exsr      exevata
051400060307     C* ...scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DEST.
051500060307     C                   exsr      exevatb
051600010205     C*
051700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
051800040802     C                   IF        FlgCAS <> '0'
051900081008     C                   IF        VABCBO = '1'
052000010205     C                   EVAL      VABCBO = '4'
052100081008     C                   ENDIF
052200081008     C                   IF        VABCBO = '2'
052300081008     C                   EVAL      VABCBO = '6'
052400010205     C                   ENDIF
052500081008     C                   ENDIF
052600020305     C*
052700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052800011113     C                   EXSR      CHKIMPDIV
052900010202     C*
053000000801     C* Ebbene...
053100000801     C                   ADD       1             §CTRMO
053200010201     C                   IF        errore <> *zeros
053300000801     C                   ADD       1             §CTRNO
053400000801     C                   EVAL      recko = vindta
053500000801     C                   EXCEPT    rigadett
053600000801     C                   EVAL      vinflg = '2'
053700000801     C                   ELSE
053800010201     C                   ADD       1             §CTROKVB
053900000801     C                   ENDIF
054000000801     C*
054100000801     C                   ENDSR
054200050628     C*----------------------------------------------------*
054300060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
054400050628     C*----------------------------------------------------*
054500060307     C     EXEVATE       BEGSR
054600050628     C*
054700050628     C                   EXSR      INZVAR
054800060705     C                   EVAL      VATAAS = VABAAS
054900060705     C                   EVAL      VATLNP = VABLNP
055000060705     C                   EVAL      VATNSP = VABNSP
055100060705     C                   EVAL      VATCCM = VABCCM
055200050628     C*
055300050628     C* Valorizzo il barcode
055400061016     C                   IF        %subst(vindta:319:3) = 'DSI' OR
055500061016     C                             %subst(vindta:319:3) = 'EEB'
055600050628     C                   EVAL      VATTRC='E'
055700070111     C                   EVAL      VATNOT = %trim(%subst(vindta:331:30))
055800070118     C                   Z-ADD     *zeros        wStep             3 0
055900070117     C                   Z-ADD     *zeros        wLen              2 0
056000070117     C                   EVAL      wLen = %len(%trim(VATNOT))
056100070117     C                   IF        wLen <= 7
056200070117     C                   EVAL      wStep = 100
056300070117     C                   ENDIF
056400070117     C                   IF        wLen  = 8
056500070117     C                   EVAL      wStep = 10
056600070117     C                   ENDIF
056700070117     C                   IF        wLen >= 9
056800070117     C                   EVAL      wStep = 1
056900070117     C                   ENDIF
057000070117     C                   MOVEL(P)  VATNOT        wBarcode          9 0
057100060801     C                   Z-ADD     0             wCollo            5 0
057200060801     C                   DOW       wCollo <  VABNCL
057300050628     C                   exsr      wrivat                                       => scarico VAT
057400060801     C                   ADD       1             wCollo
057500070117     C                   ADD       wStep         wBarcode
057600070117     C                   MOVEL(P)  wBarcode      VATNOT
057700070117     C                   EVAL      VATNOT = %subst(VATNOT:1:wLen)
057800060801     C                   ENDDO
057900060801     C                   ELSE
058000060801     C                   EVAL      VATTRC='E'
058100060801     C                   EVAL      VATNOT = %trim(%subst(vindta:331:30))
058200060801     C                   exsr      wrivat                                       => scarico VAT
058300060801     C                   ENDIF
058400050628     C*
058500050628     C                   ENDSR
058600060307     C*----------------------------------------------------*
058700060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
058800060307     C*----------------------------------------------------*
058900060307     C     EXEVATA       BEGSR
059000060307     C*
059100060307     C                   EXSR      INZVAR
059200060705     C                   EVAL      VATAAS = VABAAS
059300060705     C                   EVAL      VATLNP = VABLNP
059400060705     C                   EVAL      VATNSP = VABNSP
059500060705     C                   EVAL      VATCCM = VABCCM
059600060307     C*
059700060307     C* Valorizzo il barcode
059800060307     C                   EVAL      VATTRC='A'
059900060307     C                   EVAL      VATNOT = %trim(%subst(vindta:154:35))
060000060307     C                   exsr      wrivat                                       => scarico VAT
060100060307     C*
060200060307     C                   ENDSR
060300060307     C*----------------------------------------------------*
060400060307     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
060500060307     C*----------------------------------------------------*
060600060307     C     EXEVATB       BEGSR
060700060307     C*
060800060307     C                   EXSR      INZVAR
060900060705     C                   EVAL      VATAAS = VABAAS
061000060705     C                   EVAL      VATLNP = VABLNP
061100060705     C                   EVAL      VATNSP = VABNSP
061200060705     C                   EVAL      VATCCM = VABCCM
061300060307     C*
061400060307     C* Valorizzo il barcode
061500060307     C                   EVAL      VATTRC='B'
061600060307     C                   EVAL      VATNOT = %trim(%subst(vindta:189:20))
061700060307     C                   exsr      wrivat                                       => scarico VAT
061800060307     C*
061900060307     C                   ENDSR
062000010201     C*----------------------------------------------------*
062100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
062200010201     C*----------------------------------------------------*
062300020305     C     WRIVAT        BEGSR
062400050628     C*
062500060223     C* Scrivo solo se valorizzato qualcosa
062600060223     C                   IF        VATNOT <> *blanks
062700040802     C                   WRITE     FIVAT000
062800060223     C                   ENDIF
062900010201     C*
063000010201     C                   ENDSR
063100010202     C*----------------------------------------------------*
063200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063300010202     C*----------------------------------------------------*
063400020305     C     PREVAT        BEGSR
063500010202     C*
063600021113     C* Compongo il nome del membro da dare al FIVATWWR
063700010202     C                   eval      parmbr = vlrhdl
063800010202     C                   movel     'M'           parmbr
063900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
064000010202     C                   eval      paropz = '1'
064100010202     C* Effettuo la chiamata al CLLE preposto
064200040506     C                   call(e)   'TITVVTC'
064300010202     C                   parm                    parccm
064400010202     C                   parm                    parmbr
064500010202     C                   parm                    paropz
064600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064700010202     C                   if        %error
064800010202     C                   movel     '1'           chkcall
064900010202     C                   else
065000010202     C                   movel     '0'           chkcall
065100010202     C                   endif
065200010202     C*
065300010202     C                   ENDSR
065400000801     C*----------------------------------------------------*
065500000801     C*  CONTROLLO NUMERICITA' CAMPI
065600000801     C*----------------------------------------------------*
065700000801     C     CHKNUM        BEGSR
065800000801     C*
065900000801     C                   call(e)   'ISNUMERIC'
066000000801     C                   PARM                    PiStr            30
066100050627     C                   PARM      ','           PiDecChr          1
066200000801     C                   PARM      *ZEROS        PiVal            30 9
066300000801     C                   PARM      '0'           PiInt             1
066400000801     C                   PARM      '0'           PiNum             1
066500000801     C                   IF        %error
066600000801     C                   EVAL      PiInt=*off
066700000801     C                   ENDIF
066800000801     C*
066900000801     C                   ENDSR
067000000801     C***
067100000801
067200011113
067300011113     C*----------------------------------------------------*
067400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
067500011113     C*----------------------------------------------------*
067600011113     C     CHKIMPDIV     BEGSR
067700011113     C*
067800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067900011113     C                   Z-ADD     *zeros        wrkDec            9 9
068000011113     C*
068100011113     C* Come prima cosa effettuo considerazioni sulla divisa
068200011113     C                   IF        vabIAS > *zeros
068300011113     C                   IF        vabVAS <> 'EUR'
068400011113     C                   EVAL      vabVAS =  'ITL'
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C                   IF        vabCAS > *zeros
068900011113     C                   IF        vabVCA <> 'EUR'
069000011113     C                   EVAL      vabVCA =  'ITL'
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C                   IF        vabVMD > *zeros
069500020305     C                   IF        vabVAD <> 'EUR'
069600011113     C                   EVAL      vabVAD =  'ITL'
069700011113     C                   ENDIF
069800011113     C                   ENDIF
069900011113     C*
070000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070100011113     C                   Z-ADD     vabIAS        wrkDec
070200011113     C                   IF        wrkDec > *zeros
070300011113     C                   IF        vabVAS = 'ITL'
070400011113     C                   EVAL      vabIAS = *zeros
070500011113     C                   ENDIF
070600011113     C                   ENDIF
070700011113     C*
070800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
070900011113     C                   Z-ADD     vabCAS        wrkDec
071000011113     C                   IF        wrkDec > *zeros
071100011113     C                   IF        vabVCA = 'ITL'
071200011113     C                   EVAL      vabCAS = *zeros
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071700011113     C                   Z-ADD     vabVMD        wrkDec
071800011113     C                   IF        wrkDec > *zeros
071900011113     C                   IF        vabVAD = 'ITL'
072000011113     C                   EVAL      vabVMD = *zeros
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C                   ENDSR
072500011113     C***
072600011113
072700011113
072800000801
072900000801
073000990920      /TITLE Invio dei dati al punto operativo.
073100010202     C     invio         BEGSR
073200990920     C*
073300021113     C* 1° invio FIVAT
073400010201     C                   reset                   dscmz
073500010201     C                   move      vlrpoi        cmzdst
073600021113     C                   eval      cmzfld = 'FIVATWWR'
073700010201     C                   eval      cmzmbd = vlrhdl
073800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073900021009     C***                if        prmfir = *blanks
074000021113     C                   eval      cmzfla = 'FIVAT00F'
074100021113     C                   eval      cmzmba = 'FIVAT00F'
074200021009     C***                else
074300021009     C***                eval      cmzfla = prmfir
074400021009     C***                eval      cmzmba = prmfir
074500021009     C***                endif
074600010201     C                   eval      cmznrr = *zeros
074700020305     C                   move      §ctrokvt      cmznrr
074800021018     C                   eval      cmzlba = vlrfl1
074900010201     C                   call(e)   'TIS711C'
075000010201     C                   parm                    dscmz
075100010201     C                   parm      *blanks       esito
075200010205     C                   if        %error
075300010205     C                             or cmzerr = '1'
075400010205     C                             or esito  = '1'
075500010205     C                   eval      wrkesito = '3'
075600010205     C                   else
075700010201     C*
075800021113     C* 2° invio FIVAB
075900010201     C                   reset                   dscmz
076000010201     C                   move      vlrpoi        cmzdst
076100010201     C                   eval      cmzfld = vlrfou
076200010201     C                   eval      cmzmbd = vlrhdl
076300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076400021009     C***                if        prmfir = *blanks
076500021113     C                   eval      cmzfla = 'FIVAB00F'
076600021113     C                   eval      cmzmba = 'FIVAB00F'
076700021009     C***                else
076800021009     C***                eval      cmzfla = prmfir
076900021009     C***                eval      cmzmba = prmfir
077000021009     C***                endif
077100010201     C                   eval      cmznrr = *zeros
077200010201     C                   move      §ctrokvb      cmznrr
077300021018     C                   eval      cmzlba = vlrfl1
077400010201     C                   call(e)   'TIS711C'
077500010201     C                   parm                    dscmz
077600010201     C                   parm      *blanks       esito
077700010201     C                   if        %error
077800010201     C                             or cmzerr = '1'
077900010201     C                             or esito  = '1'
078000010201     C                   eval      wrkesito = '3'
078100010201     C                   endif
078200010205     C                   endif
078300990920     C*
078400000613     C                   ENDSR
078500000613     C***
078600070411
078700070411     C     *pssr         BEGSR
078800070411     C*
078900070411     C                   if        %open(tivin00r)
079000070411     C                   close     tivin00r
079100070411     C                   endif
079200070411     C                   if        %open(fivabwwr)
079300070411     C                   close     fivabwwr
079400070411     C                   endif
079500070411     C                   if        %open(fivatwwr)
079600070411     C                   close     fivatwwr
079700070411     C                   endif
079800070411     C*
079900070411     C* Effettuo la chiamata al CLLE preposto
080000070411     C                   call(e)   'TITVVTC'
080100070411     C                   parm                    parccm
080200070411     C                   parm                    parmbr
080300070411     C                   parm      '2'           paropz
080400070411     C*
080500070411     C                   eval      wrkesito = '2'
080600070411     C*
080700070411     C                   seton                                        LR
080800070411     C*
080900070411     C                   ENDSR     '*CANCL'
081000070411     C***
081100070411
081200990910
081300000613     C     *inzsr        BEGSR
081400990910     C*
081500990910     C     *entry        plist
081600990920     C                   parm                    tivlrds
081700990921     C                   parm      wrkesito      esito
081800000724     C                   parm                    prmlit
081900000710     C                   parm                    prmfir
082000000613     C*
082100000830     C* CALCOLA LA DATA CORRENTE
082200000830     C                   time                    wn14             14 0
082300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
082400000830     C                   z-add     wn8           g08dat
082500000830     C                   z-add     *zeros        g08inv
082600000830     C                   movel     '0'           g08err
082700000830     C                   call      'XSRDA8'
082800000830     C                   parm                    wlbda8
082900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
083000000830     C*
083100000613     C                   ENDSR
083200000613     C***
083300990908
083400060307     Otitvr7p   E            riepilogo         2
083500990915     O                                              'Upload via Internet'
083600990915     O                                           +1 'Traduzione TIVIN00R -'
083700021113     O                                           +1 'FIVABWWR/FIVATWWR'
083800010201     O                                           +1 'Report testate bolle'
083900990915     O          E            riepilogo   2
084000990915     O                       wrkdata
084100990915     O                       wrkora              +1
084200990915     O                       procname            +1
084300990915     O          E            riepilogo   2
084400990915     O                                              'Cliente..................:'
084500990915     O                       VABCCM        z     +1
084600990915     O          E            riepilogo   2
084700990920     O                                              'Riferimento Strategi.....:'
084800990920     O                       vlrhdl              +1
084900990915     O          E            riepilogo   2
085000990915     O                                              'Giusti...................:'
085100010201     O                       §CTROKVB      2   +  1
085200990915     O          E            riepilogo   2
085300990915     O                                              'Sbagliati e corretti.....:'
085400971022     O                       §CTRMO        2   +  1
085500990915     O          E            riepilogo   2
085600990915     O                                              'Sbagliati e scartati.....:'
085700971022     O                       §CTRNO        2   +  1
085800000613
085900060307     Otitvr7ps  E            testdett          2
086000000613     O                                              'Upload via Internet'
086100000613     O                                           +1 'Traduzione TIVIN00R -'
086200021113     O                                           +1 'FIVABWWR/FIVATWWR'
086300010201     O                                           +1 'Report testate bolle'
086400000616     O          E            testdett    3
086500000613     O                                           +2 'N° rec'
086600000613     O                                           +3 'Anteprima contenuto'
086700000616     O          E            rigadett    2
086800000613     O                       rrnum               +2
086900000621     O                       recko               +3
087000000616     O          E            findett     2
087100000613     O                       wrkdata
087200000613     O                       wrkora              +1
087300000613     O                       procname            +1
087400000616     O          E            findett     2
087500000613     O                                              'Cliente..................:'
087600000613     O                       VABCCM        z     +1
087700000616     O          E            findett     2
087800000613     O                                              'Riferimento Strategi.....:'
087900000613     O                       vlrhdl              +1
088000000616     O          E            findett     2
088100000613     O                                              'Giusti...................:'
088200010201     O                       §CTROKVB      2   +  1
088300000616     O          E            findett     2
088400000613     O                                              'Sbagliati e corretti.....:'
088500000613     O                       §CTRMO        2   +  1
088600000616     O          E            findett     2
088700000613     O                                              'Sbagliati e scartati.....:'
088800000613     O                       §CTRNO        2   +  1
088900000512** CMD - COMANDI CL
089000060307OVRPRTF FILE(TITVR7P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
089100060307OVRPRTF FILE(TITVR7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
089200060307DLTOVR FILE(TITVR7P TITVR7PS) LVL(*)
089300000512
089400000512
