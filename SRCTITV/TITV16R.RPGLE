000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800020619     Ftitv16p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100020619     Ftitv16ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500020305     D depspe          s             14    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700010213     D depaas          s              4    INZ(*zeros)
003800020305     D curspe          s             14    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300010213
004400000801     D  Num5_0         s              5  0
004500041102
004600041102     d wpos            s              5  0
004700041102     d wpos1           s              5  0
004800041102     d savpos          s              5  0
004900041102     d wcolli          s              5  0
005000000830
005100000830     D*------------------
005200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300000830     D*------------------
005400000830     D WLBDA8          DS                  INZ
005500000830     D  G08DAT                 1      8  0
005600000830     D  G08INV                 9     16  0
005700000830     D  G08ERR                17     17
005800000830     D  G08TGI                18     22  0
005900000830     D*
006000990908
006100010201
006200010201
006300990915     C                   time                    wrkdata
006400990915     C                   time                    wrkora
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800000724     C*
006900000724     C* SE OCCORRE SPEDIRE IN FILIALE
007000000724     C                   if        vlrpoi <> *zeros
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400000724     C     vlrpoi        chain     azorg01l
007500000724     C                   if        %found
007600000616     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000616     C*
007900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000616     C                   PARM                    depcmd
008300000616     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000724     C                   endif
008700000616     C*
008800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000616     C                   movel(p)  CMD(2)        depcmd
009000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000616     C*
009800020619     C                   if        not %open(titv16ps)
009900020619     C                   open      titv16ps
010000000616     C                   except    testdett
010100000613     C                   endif
010200000613     C*
010300020619     C                   EXSR      RWDVD                                        LETT/SCR. VAB
010400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010500000613     C*
010600010202     C* Effettuo la chiamata al CLLE preposto
010700050128     C                   call(e)   'TITVVTC'
010800010202     C                   parm                    parccm
010900010202     C                   parm                    parmbr
011000010202     C                   parm      '2'           paropz
011100010202     C*
011200020619     C                   if        %open(titv16ps)
011300000616     C                   except    findett
011400020619     C                   close     titv16ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300000801     C
012400010201     C                   seton                                        LR
012500000613
012600000613
012700000613     C*--------------------------------------------------------
012800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012900000613     C*--------------------------------------------------------
013000000621     C     STPR          BEGSR
013100000613     C*
013200020619     C                   if        not %open(titv16p)
013300020619     C                   open      titv16p
013400990915     C                   endif
013500990915     C*
013600990915     C                   except    riepilogo
013700990915     C*
013800020619     C                   if        %open(titv16p)
013900020619     C                   close     titv16p
014000990914     C                   endif
014100990910     C*
014200000613     C                   ENDSR
014300000613     C***
014400990908
014500000801
014600910830     C*--------------------------------------------------------
014700021113     C* RWDVD    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014800910830     C*--------------------------------------------------------
014900020619     C     RWDVD         BEGSR
015000990910     C*
015100990914     C                   if        not %open(tivin00r)
015200990908     C                   open      tivin00r
015300990914     C                   endif
015400021113     C                   if        not %open(fivabwwr)
015500021113     C                   open      fivabwwr
015600990914     C                   endif
015700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800020305     C                   exsr      prevat
015900010201     C*
016000010202     C                   if        chkcall = '0'
016100010202     C*
016200021113     C                   if        not %open(fivatwwr)
016300021113     C                   open      fivatwwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   clear                   §CTROKVB          5 0
016700020305     C                   clear                   §CTROKVT          5 0
016800000801     C                   clear                   §CTRMO            5 0
016900000801     C                   clear                   §CTRNO            5 0
017000990910     C*
017100921023     C                   DO        *HIVAL
017200990913     C*
017300990915     C                   READ      tivin00r                               70
017400000905     C                   if        vindta > *blanks
017500000613     C                   add       1             rrnum
017600000801     C*
017700000801     C                   if        *in70 = *off
017800000801     C                             and
017900000801     C                             (vinflg = *blanks
018000000801     C                              or vinflg = '0'
018100000801     C                              or vinflg = '2')
018200000801     C*
018300000801     C                   clear                   vinmsg
018400000801     C                   eval      vinflg = '1'
018500010305     C*
018600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018700020619     C                   EVAL      PiStr=%trim(%subst(vindta:5:14))
018800041102     C                   MOVEL(p)  PiStr         curspe
018900041102      * cerco il primo byte a blank
019000041102     c                   Eval      wpos = %scan(' ':curspe)
019100041102      * se non trovo nessun byte vuoto imposto la lunghezza max del campo
019200041102     c                   If        wpos = 0
019300041102     c                   eval      wpos = 14
019400041102     c                   EndIf
019500041102
019600041102     c                   Eval      wpos1 = wpos - 3
019700041102     c                   Eval      savpos = wpos1
019800041102
019900041102     C                   EVAL      PiStr=%trim(%subst(vindta:5:wpos1))
020000041102     C                   MOVEL(p)  PiStr         curspe
020100010305     C*
020200010305     C                   if        depspe = *blanks                             => 1° giro
020300010305     C                   eval      depspe = curspe                              => memorizz. spediz
020400020305     C                   exsr      impvab
020500020305     C                   exsr      wrivat                                       => carico VAT
020600010305     C                   else
020700010305     C                   if        curspe <> depspe                             => rottura di spediz
020800010305     C                   eval      depspe = curspe                              => memorizz. spediz
020900020305     C                   exsr      wrivab
021000041102     c                   Clear                   wcolli
021100020305     C                   exsr      impvab
021200020305     C                   exsr      wrivat                                       => carico VAT
021300020305     C                   else                                                   => x stessa spediz
021400020305     C                   exsr      impvab
021500020305     C                   exsr      wrivat                                       => carico VAT
021600010305     C                   endif
021700010305     C                   endif
021800010305     C                   endif
021900000905     C*
022000000905     C                   else
022100000905     C                   eval      vinflg = '1'
022200000905     C                   endif
022300000905     C*
022400000905     C  N70              update    tivin000
022500000905     C*
022600991022     C  N70              ENDdo
022700010202     C*
022800020305     C* Scarico i VAB rimasti "in sospeso"
022900020305     C                   exsr      wrivab
023000010202     C*
023100010202     C                   endif
023200990910
023300990910     C* Se non ci sono record con errori ...
023400000710     C                   if        §ctrno = 0
023500990910     C* ... restituisco esito OK.
023600990921     C                   eval      wrkesito = '0'
023700990910     C                   else
023800010201     C                   if        §ctrokvb > 0
023900990921     C                   eval      wrkesito = '1'
024000000710     C                   else
024100000710     C                   eval      wrkesito = '2'
024200990910     C                   endif
024300000710     C                   endif
024400990910     C*
024500990914     C                   if        %open(tivin00r)
024600990908     C                   close     tivin00r
024700990914     C                   endif
024800021113     C                   if        %open(fivabwwr)
024900021113     C                   close     fivabwwr
025000990914     C                   endif
025100021113     C                   if        %open(fivatwwr)
025200021113     C                   close     fivatwwr
025300010201     C                   endif
025400990910     C*
025500010201     C                   if        §ctrokvb > 0
025600000724     C                             and vlrpoi <> *zeros
025700010202     C                   exsr      invio
025800990920     C                   endif
025900990920     C*
026000910830     C                   ENDSR
026100000613     C***
026200010305
026300010305     C*----------------------------------------------------*
026400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026500010305     C*----------------------------------------------------*
026600020305     C     WRIVAB        BEGSR
026700010305     C*
026800021113     C                   write     fivab000                                     => scarico il VAB
026900010305     C*
027000010305     C                   ENDSR
027100990920
027200000801     C*----------------------------------------------------*
027300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027400000801     C*----------------------------------------------------*
027500010201     C     INZVAR        BEGSR
027600000801     C*
027700010201     C                   Z-ADD     *zeros        Num5_0
027800080606     C                   MOVEL     '0'           FlgCAS            1
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028300000801     C*----------------------------------------------------*
028400000801     C     DEFCAM        BEGSR
028500000801     C*
028600021113     C                   CLEAR                   FIVAB000
028700020619     C* Imposto i valori di default...
028800020619     C                   Z-ADD     0892176       VABCCM
028900020619     C                   Z-ADD     0892176       VATCCM
029000020619     C                   Z-ADD     089           VABLNP
029100020619     C                   Z-ADD     089           VATLNP
029200020619     C***                Z-ADD     000           VABCTR
029300020305     C                   MOVEL     '7Q'          VABCTM
029400080606     C                   MOVEL     '1'           VABCBO
029500080606     C                   MOVEL     'E'           VATTRC
029600020619     C* ... e poi verifico se sono stati passati come parametri
029700020619     C                   IF        vlrppt > *blanks
029800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029900020619     C                   EXSR      CHKNUM
030000020619     C                   IF        PiInt=*on
030100020619     C                   Z-ADD     PiVal         VABCCM
030200020619     C                   Z-ADD     PiVal         VATCCM
030300020619     C                   ENDIF
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABLNP
030800020619     C                   Z-ADD     PiVal         VATLNP
030900020619     C                   ENDIF
031000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031100020619     C                   EXSR      CHKNUM
031200020619     C                   IF        PiInt=*on
031300020619     C                   Z-ADD     PiVal         VABCTR
031400020619     C                   ENDIF
031500020619     C                   ENDIF
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
032000000801     C*----------------------------------------------------*
032100010201     C     IMPVAB        BEGSR
032200000801     C*
032300020305     C                   EXSR      INZVAR
032400020305     C                   EXSR      DEFCAM
032500010305     C*
032600000801     C                   Z-ADD     *zeros        errore            1 0
032700000830     C                   MOVEL     datcor        VABAAS
032800020305     C                   MOVEL     datcor        VATAAS
032900021113     C                   MOVE(P)   vlrpoi        VABFGS
033000021113     C                   MOVE(P)   vlrpoi        VATFGS
033100010213     C                   MOVEL     VABAAS        depaas
033200000830     C                   MOVE      datcor        VABMGS
033300020619     C                   EVAL      VABRSD=%trim(%subst(vindta:34:30))
033400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033600020117     C* ==
033700031124     C                   EVAL      VABRD2=%trim(%subst(vindta:94:30))
033800020619     C                   EVAL      VABIND=%trim(%subst(vindta:64:30))
033900020619     C                   EVAL      VABLOD=%trim(%subst(vindta:124:30))
034000020619     C                   EVAL      VABPRD=%trim(%subst(vindta:154:30))
034100020619     C                   EVAL      VABCAD=%trim(%subst(vindta:184:9))
034200020619     C                   EVAL      VABNZD=%trim(%subst(vindta:193:3))
034300020918     C                   IF        %trim(VABNZD) = 'IT'
034400020918     C                   EVAL      VABNZD=*blanks
034500020918     C                   ENDIF
034600050701     C* CCM (se presente)
034700050701     C                   IF        %trim(%subst(vindta:19:7)) <> *blanks
034800050701     C                   EVAL      PiStr=%trim(%subst(vindta:19:7))
034900050701     C                   EXSR      CHKNUM
035000050701     C                   IF        PiInt=*on
035100050701     C                   Z-ADD     PiVal         VABCCM
035200050713     C                   Z-ADD     PiVal         VATCCM
035300050701     C                   ENDIF
035400050701     C                   ENDIF
035500041102
035600041102      * riferimento alfa --> devo togliere il n. dei colli
035700041102     C                   EVAL      Pistr=%trim(%subst(vindta:212:18))
035800041102     c                   Eval      wpos = %scan(' ':pistr)
035900041102      * se non trovo nessun byte vuoto imposto la lunghezza max del campo
036000041102     c                   If        wpos = 0
036100041102     c                   eval      wpos = 18
036200041102     c                   EndIf
036300041102
036400041102     c                   Eval      wpos1 = wpos - 3
036500041102
036600041102     C                   EVAL      VABRMA=%trim(%subst(vindta:212:wpos1))
036700041102
036800041102     C* n. colli --> conto i record per spedizione
036900041102     c                   add       1             wcolli
037000041102     c                   Move      wcolli        VABNCL
037100041102     C**!!!              EVAL      PiStr=%trim(%subst(vindta:312:4))
037200041102     C**!!!              EXSR      CHKNUM
037300041102     C**!!!              IF        PiInt=*on
037400041102     C**!!!              Z-ADD     PiVal         Num5_0
037500041102     C**!!!              MOVE      Num5_0        VABNCL
037600041102     C**!!!              ELSE
037700041102     C**!!!              ADD       1             errore
037800041102     C**!!!              EVAL      vinmsg = %trimr(vinmsg)
037900041102     C**!!!                        + ' ' + 'VABNCL'
038000041102     C**!!!              ENDIF
038100020305     C*
038200020626     C                   EVAL      PiStr=%trim(%subst(vindta:304:8))
038300010201     C                   EXSR      CHKNUM
038400010201     C                   IF        PiNum=*on
038500011115     C                   Z-ADD     PiVal         VABPKB
038600010201     C                   ELSE
038700010201     C                   ADD       1             errore
038800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038900010201     C                             + ' ' + 'VABPKB'
039000010201     C                   ENDIF
039100041102      * riferimento numerico --> devo togliere il n. dei colli
039200041102     C                   EVAL      PiStr=%trim(%subst(vindta:1:18))
039300041102     c                   Eval      wpos = %scan(' ':pistr)
039400041102      * se non trovo nessun byte vuoto imposto la lunghezza max del campo
039500041102     c                   If        wpos = 0
039600041102     c                   eval      wpos = 18
039700041102     c                   EndIf
039800041102     c                   Eval      wpos1 = wpos - 3
039900041102
040000041102     C                   EVAL      pistr=%trim(%subst(vindta:1:wpos1))
040100010201     C                   EXSR      CHKNUM
040200010201     C                   IF        PiInt=*on
040300011115     C                   Z-ADD     PiVal         VABRMN
040400010201     C                   ELSE
040500010201     C                   ADD       1             errore
040600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
040700010201     C                             + ' ' + 'VABRMN'
040800010201     C                   ENDIF
040900020619     C*
041000041102      * numero spedizione --> devo togliere mmdd e il n. dei colli
041100041102     C                   EVAL      PiStr=%trim(%subst(vindta:5:savpos))
041200041102     C**!!!              EVAL      PiStr=%trim(%subst(vindta:5:14))
041300010201     C                   EXSR      CHKNUM
041400010201     C                   IF        PiInt=*on
041500011115     C                   Z-ADD     PiVal         VABNSP
041600020305     C                   Z-ADD     PiVal         VATNSP
041700010201     C                   ELSE
041800010201     C                   ADD       1             errore
041900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
042000020305     C                             + ' ' + 'VABNSP VATNSP'
042100010201     C                   ENDIF
042200020619     C*
042300020626     C                   IF        %trim(%subst(vindta:230:60)) = 'CONTRASSEGNO'
042400080606     C                   MOVEL     '1'           FlgCAS
042500020626     C                   EVAL      PiStr=%trim(%subst(vindta:293:11))
042600020619     C                   EXSR      CHKNUM
042700020626     C                   IF        PiNum=*on
042800020619     C                   Z-ADD     PiVal         VABCAS
042900020619     C                   EVAL      VABVCA=%trim(%subst(vindta:290:3))
043000020619     C                   ELSE
043100020619     C                   ADD       1             errore
043200020619     C                   EVAL      vinmsg = %trimr(vinmsg)
043300020619     C                             + ' ' + 'VABCAS'
043400020619     C                   ENDIF
043500020626     C                   ENDIF
043600030728     C*
043700030728     C                   IF        %trim(%subst(vindta:316:11)) = '00000000001'
043800030729     C                             OR %subst(vindta:316:11) = *blanks
043900030728     C                   ELSE
044000030728     C                   EVAL      PiStr=%trim(%subst(vindta:316:11))
044100030728     C                   EXSR      CHKNUM
044200030728     C                   IF        PiNum=*on
044300030728     C                   Z-ADD     PiVal         VABIAS
044400030728     C                   EVAL      VABVAS=%trim(%subst(vindta:290:3))
044500030728     C                   ELSE
044600030728     C                   ADD       1             errore
044700030728     C                   EVAL      vinmsg = %trimr(vinmsg)
044800030728     C                             + ' ' + 'VABIAS'
044900030728     C                   ENDIF
045000030728     C                   ENDIF
045100010205     C*
045200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
045300080606     C                   IF        FlgCAS = '1'
045400080606     C                   IF        VABCBO = '1'
045500010205     C                   EVAL      VABCBO = '4'
045600080606     C                   ENDIF
045700080606     C                   IF        VABCBO = '2'
045800080606     C                   EVAL      VABCBO = '6'
045900010205     C                   ENDIF
046000080606     C                   ENDIF
046100020305     C*
046200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046300011113     C                   EXSR      CHKIMPDIV
046400010202     C*
046500000801     C* Ebbene...
046600000801     C                   ADD       1             §CTRMO
046700010201     C                   IF        errore <> *zeros
046800000801     C                   ADD       1             §CTRNO
046900000801     C                   EVAL      recko = vindta
047000000801     C                   EXCEPT    rigadett
047100000801     C                   EVAL      vinflg = '2'
047200000801     C                   ELSE
047300010201     C                   ADD       1             §CTROKVB
047400000801     C                   ENDIF
047500000801     C*
047600000801     C                   ENDSR
047700010201     C*----------------------------------------------------*
047800021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047900010201     C*----------------------------------------------------*
048000020305     C     WRIVAT        BEGSR
048100010201     C*
048200021113     C* Valorizzo l buffer di scrittura del FIVAT
048300020619     C                   EVAL      VATNOT = %subst(vindta:1:18)
048400021113     C                   write     FIVAT000
048500040224     C                   EVAL      VATTRC = 'B'
048600040224     C                   EVAL      VATNOT=%subst(vindta:196:16)
048700040224     C                   write     FIVAT000
048800010201     C*
048900010201     C                   ENDSR
049000010202     C*----------------------------------------------------*
049100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049200010202     C*----------------------------------------------------*
049300020305     C     PREVAT        BEGSR
049400010202     C*
049500021113     C* Compongo il nome del membro da dare al FIVATWWR
049600010202     C                   eval      parmbr = vlrhdl
049700010202     C                   movel     'M'           parmbr
049800020619     C                   eval      parccm = '00892176'
049900010202     C                   eval      paropz = '1'
050000010202     C* Effettuo la chiamata al CLLE preposto
050100050128     C                   call(e)   'TITVVTC'
050200010202     C                   parm                    parccm
050300010202     C                   parm                    parmbr
050400010202     C                   parm                    paropz
050500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600010202     C                   if        %error
050700010202     C                   movel     '1'           chkcall
050800010202     C                   else
050900010202     C                   movel     '0'           chkcall
051000010202     C                   endif
051100010202     C*
051200010202     C                   ENDSR
051300000801     C*----------------------------------------------------*
051400000801     C*  CONTROLLO NUMERICITA' CAMPI
051500000801     C*----------------------------------------------------*
051600000801     C     CHKNUM        BEGSR
051700000801     C*
051800000801     C                   call(e)   'ISNUMERIC'
051900000801     C                   PARM                    PiStr            30
052000020619     C                   PARM      '.'           PiDecChr          1
052100000801     C                   PARM      *ZEROS        PiVal            30 9
052200000801     C                   PARM      '0'           PiInt             1
052300000801     C                   PARM      '0'           PiNum             1
052400000801     C                   IF        %error
052500000801     C                   EVAL      PiInt=*off
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900000801     C***
053000000801
053100011113
053200011113     C*----------------------------------------------------*
053300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053400011113     C*----------------------------------------------------*
053500011113     C     CHKIMPDIV     BEGSR
053600011113     C*
053700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053800011113     C                   Z-ADD     *zeros        wrkDec            9 9
053900011113     C*
054000011113     C* Come prima cosa effettuo considerazioni sulla divisa
054100011113     C                   IF        vabIAS > *zeros
054200011113     C                   IF        vabVAS <> 'EUR'
054300011113     C                   EVAL      vabVAS =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabCAS > *zeros
054800011113     C                   IF        vabVCA <> 'EUR'
054900011113     C                   EVAL      vabVCA =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabVMD > *zeros
055400020305     C                   IF        vabVAD <> 'EUR'
055500011113     C                   EVAL      vabVAD =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056000011113     C                   Z-ADD     vabIAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAS = 'ITL'
056300011113     C                   EVAL      vabIAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056800011113     C                   Z-ADD     vabCAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVCA = 'ITL'
057100011113     C                   EVAL      vabCAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057600011113     C                   Z-ADD     vabVMD        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAD = 'ITL'
057900011113     C                   EVAL      vabVMD = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   ENDSR
058400011113     C***
058500011113
058600011113
058700000801
058800000801
058900990920      /TITLE Invio dei dati al punto operativo.
059000010202     C     invio         BEGSR
059100990920     C*
059200021113     C* 1° invio FIVAT
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500021113     C                   eval      cmzfld = 'FIVATWWR'
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAT00F'
060000021113     C                   eval      cmzmba = 'FIVAT00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600020305     C                   move      §ctrokvt      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010205     C                   if        %error
061200010205     C                             or cmzerr = '1'
061300010205     C                             or esito  = '1'
061400010205     C                   eval      wrkesito = '3'
061500010205     C                   else
061600010201     C*
061700021113     C* 2° invio FIVAB
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000010201     C                   eval      cmzfld = vlrfou
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400021113     C                   eval      cmzfla = 'FIVAB00F'
062500021113     C                   eval      cmzmba = 'FIVAB00F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100010201     C                   move      §ctrokvb      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010201     C                   if        %error
063700010201     C                             or cmzerr = '1'
063800010201     C                             or esito  = '1'
063900010201     C                   eval      wrkesito = '3'
064000010201     C                   endif
064100010205     C                   endif
064200990920     C*
064300000613     C                   ENDSR
064400000613     C***
064500070411
064600070411     C     *pssr         BEGSR
064700070411     C*
064800070411     C                   if        %open(tivin00r)
064900070411     C                   close     tivin00r
065000070411     C                   endif
065100070411     C                   if        %open(fivabwwr)
065200070411     C                   close     fivabwwr
065300070411     C                   endif
065400070411     C                   if        %open(fivatwwr)
065500070411     C                   close     fivatwwr
065600070411     C                   endif
065700070411     C*
065800070411     C* Effettuo la chiamata al CLLE preposto
065900070411     C                   call(e)   'TITVVTC'
066000070411     C                   parm                    parccm
066100070411     C                   parm                    parmbr
066200070411     C                   parm      '2'           paropz
066300070411     C*
066400070411     C                   eval      wrkesito = '2'
066500070411     C*
066600070411     C                   seton                                        LR
066700070411     C*
066800070411     C                   ENDSR     '*CANCL'
066900070411     C***
067000070411
067100990910
067200000613     C     *inzsr        BEGSR
067300990910     C*
067400990910     C     *entry        plist
067500990920     C                   parm                    tivlrds
067600990921     C                   parm      wrkesito      esito
067700000724     C                   parm                    prmlit
067800000710     C                   parm                    prmfir
067900000613     C*
068000000830     C* CALCOLA LA DATA CORRENTE
068100000830     C                   time                    wn14             14 0
068200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068300000830     C                   z-add     wn8           g08dat
068400000830     C                   z-add     *zeros        g08inv
068500000830     C                   movel     '0'           g08err
068600000830     C                   call      'XSRDA8'
068700000830     C                   parm                    wlbda8
068800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068900000830     C*
069000000613     C                   ENDSR
069100000613     C***
069200990908
069300020619     Otitv16p   E            riepilogo         2
069400990915     O                                              'Upload via Internet'
069500990915     O                                           +1 'Traduzione TIVIN00R -'
069600021113     O                                           +1 'FIVABWWR/FIVATWWR'
069700010201     O                                           +1 'Report testate bolle'
069800990915     O          E            riepilogo   2
069900990915     O                       wrkdata
070000990915     O                       wrkora              +1
070100990915     O                       procname            +1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Cliente..................:'
070400990915     O                       VABCCM        z     +1
070500990915     O          E            riepilogo   2
070600990920     O                                              'Riferimento Strategi.....:'
070700990920     O                       vlrhdl              +1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Giusti...................:'
071000010201     O                       §CTROKVB      2   +  1
071100990915     O          E            riepilogo   2
071200990915     O                                              'Sbagliati e corretti.....:'
071300971022     O                       §CTRMO        2   +  1
071400990915     O          E            riepilogo   2
071500990915     O                                              'Sbagliati e scartati.....:'
071600971022     O                       §CTRNO        2   +  1
071700000613
071800020619     Otitv16ps  E            testdett          2
071900000613     O                                              'Upload via Internet'
072000000613     O                                           +1 'Traduzione TIVIN00R -'
072100021113     O                                           +1 'FIVABWWR/FIVATWWR'
072200010201     O                                           +1 'Report testate bolle'
072300000616     O          E            testdett    3
072400000613     O                                           +2 'N° rec'
072500000613     O                                           +3 'Anteprima contenuto'
072600000616     O          E            rigadett    2
072700000613     O                       rrnum               +2
072800000621     O                       recko               +3
072900000616     O          E            findett     2
073000000613     O                       wrkdata
073100000613     O                       wrkora              +1
073200000613     O                       procname            +1
073300000616     O          E            findett     2
073400000613     O                                              'Cliente..................:'
073500000613     O                       VABCCM        z     +1
073600000616     O          E            findett     2
073700000613     O                                              'Riferimento Strategi.....:'
073800000613     O                       vlrhdl              +1
073900000616     O          E            findett     2
074000000613     O                                              'Giusti...................:'
074100010201     O                       §CTROKVB      2   +  1
074200000616     O          E            findett     2
074300000613     O                                              'Sbagliati e corretti.....:'
074400000613     O                       §CTRMO        2   +  1
074500000616     O          E            findett     2
074600000613     O                                              'Sbagliati e scartati.....:'
074700000613     O                       §CTRNO        2   +  1
074800000512** CMD - COMANDI CL
074900020619OVRPRTF FILE(TITV16P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075000020619OVRPRTF FILE(TITV16PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075100020619DLTOVR FILE(TITV16P TITV16PS) LVL(*)
075200000512
075300000512
