000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800041026     Ftitvf5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100041026     Ftitvf5ps  O    f  198        PRINTER usropn
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
002600040506     D tisi95ds      e ds
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
004000041025     D depspe          s             10    INZ(*blanks)
004100041025     D curspe          s             10    INZ(*blanks)
004200000830
004300000830     D*------------------
004400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500000830     D*------------------
004600000830     D WLBDA8          DS                  INZ
004700000830     D  G08DAT                 1      8  0
004800000830     D  G08INV                 9     16  0
004900000830     D  G08ERR                17     17
005000000830     D  G08TGI                18     22  0
005100000830     D*
005200990908
005300010201
005400010201
005500990915     C                   time                    wrkdata
005600990915     C                   time                    wrkora
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000000724     C*
006100000724     C* SE OCCORRE SPEDIRE IN FILIALE
006200000724     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600000724     C     vlrpoi        chain     azorg01l
006700000724     C                   if        %found
006800000616     C                   movel(p)  CMD(1)        depcmd
006900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007000000616     C*
007100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007200011113     C                   Z-ADD     150           LENGH            15 5
007300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007400000616     C                   PARM                    depcmd
007500000616     C                   PARM                    LENGH
007600000724     C*
007700000724     C                   endif
007800000724     C                   endif
007900000616     C*
008000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008100000616     C                   movel(p)  CMD(2)        depcmd
008200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008300000616     C*
008400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008500011113     C                   Z-ADD     150           LENGH            15 5
008600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008700000616     C                   PARM                    depcmd
008800000616     C                   PARM                    LENGH
008900000616     C*
009000041026     C                   if        not %open(titvf5ps)
009100041026     C                   open      titvf5ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900040506     C                   call(e)   'TITVVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300050201     C*
010400050201     C* Effettuo lancio TISI95 solo x chiusura
010500050201     C                   CLEAR                   TISI95DS
010600050201     C                   EVAL      I95TLA = 'C'
010700050201     C                   CALL      'TISI95R'
010800050201     C                   PARM                    TISI95DS
010900010202     C*
011000041026     C                   if        %open(titvf5ps)
011100000616     C                   except    findett
011200041026     C                   close     titvf5ps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600011113     C                   Z-ADD     150           LENGH            15 5
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000616     C                   PARM                    CMD(3)
011900000616     C                   PARM                    LENGH
012000000616     C*
012100000801     C
012200010201     C                   seton                                        LR
012300000613
012400000613
012500000613     C*--------------------------------------------------------
012600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012700000613     C*--------------------------------------------------------
012800000621     C     STPR          BEGSR
012900000613     C*
013000041026     C                   if        not %open(titvf5p)
013100041026     C                   open      titvf5p
013200990915     C                   endif
013300990915     C*
013400990915     C                   except    riepilogo
013500990915     C*
013600041026     C                   if        %open(titvf5p)
013700041026     C                   close     titvf5p
013800990914     C                   endif
013900990910     C*
014000000613     C                   ENDSR
014100000613     C***
014200990908
014300000801
014400910830     C*--------------------------------------------------------
014500040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014600910830     C*--------------------------------------------------------
014700040526     C     RWFILE        BEGSR
014800990910     C*
014900990914     C                   if        not %open(tivin00r)
015000990908     C                   open      tivin00r
015100990914     C                   endif
015200021113     C                   if        not %open(fivabwwr)
015300021113     C                   open      fivabwwr
015400990914     C                   endif
015500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600020305     C                   exsr      prevat
015700010201     C*
015800010202     C                   if        chkcall = '0'
015900010202     C*
016000021113     C                   if        not %open(fivatwwr)
016100021113     C                   open      fivatwwr
016200010201     C                   endif
016300990910     C*
016400010201     C                   clear                   §CTROKVB          5 0
016500020305     C                   clear                   §CTROKVT          5 0
016600000801     C                   clear                   §CTRMO            5 0
016700000801     C                   clear                   §CTRNO            5 0
016800990910     C*
016900921023     C                   DO        *HIVAL
017000990913     C*
017100990915     C                   READ      tivin00r                               70
017200040823     C                   if        vindta > *blanks                AND
017300040823     C                             %subst(vindta:1:7) <> 'barcode' AND
017400040823     C                             %subst(vindta:1:7) <> 'BARCODE'
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
018600040823     C* Verifico la rottura d spedizione
018700041025     C                   eval      curspe = %subst(vindta:187:10)
018800040823     C                   if        depspe <> curspe
018900040823     C* Se NN trattasi d 1° giro scarico il buffer del "VAB" precedente
019000040823     C                   if        depspe <> *blanks
019100040802     C                   exsr      wrivab
019200040823     C                   endif
019300040823     C* Imposto il "VAB" corrente
019400040823     C                   exsr      impvab
019500040823     C* Salvo la spedizione corrente
019600040823     C                   eval      depspe = curspe
019700040823     C                   endif
019800040823     C*
019900040823     C* Ad ogni record scrivo sempre e cmq il "VAT"
020000040823     C                   exsr      wrivat
020100040802     C*
020200010305     C                   endif
020300000905     C*
020400000905     C                   else
020500000905     C                   eval      vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800000905     C  N70              update    tivin000
020900000905     C*
021000991022     C  N70              ENDdo
021100040823     C*
021200040823     C* Al termine della lettura del file scarico il buffer del "VAB" rimasto "in canna"
021300040823     C                   exsr      wrivab
021400010202     C*
021500010202     C                   endif
021600990910
021700990910     C* Se non ci sono record con errori ...
021800000710     C                   if        §ctrno = 0
021900990910     C* ... restituisco esito OK.
022000990921     C                   eval      wrkesito = '0'
022100990910     C                   else
022200010201     C                   if        §ctrokvb > 0
022300990921     C                   eval      wrkesito = '1'
022400000710     C                   else
022500000710     C                   eval      wrkesito = '2'
022600990910     C                   endif
022700000710     C                   endif
022800990910     C*
022900990914     C                   if        %open(tivin00r)
023000990908     C                   close     tivin00r
023100990914     C                   endif
023200021113     C                   if        %open(fivabwwr)
023300021113     C                   close     fivabwwr
023400990914     C                   endif
023500021113     C                   if        %open(fivatwwr)
023600021113     C                   close     fivatwwr
023700010201     C                   endif
023800990910     C*
023900010201     C                   if        §ctrokvb > 0
024000000724     C                             and vlrpoi <> *zeros
024100010202     C                   exsr      invio
024200990920     C                   endif
024300990920     C*
024400910830     C                   ENDSR
024500000613     C***
024600010305
024700010305     C*----------------------------------------------------*
024800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
024900010305     C*----------------------------------------------------*
025000020305     C     WRIVAB        BEGSR
025100010305     C*
025200021113     C                   write     fivab000                                     => scarico il VAB
025300010305     C*
025400010305     C                   ENDSR
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100040802     C                   Z-ADD     *zeros        Num5_0            5 0
026200040802     C                   MOVEL     '0'           FlgCAS            1
026300000801     C*
026400000801     C                   ENDSR
026500000801     C*----------------------------------------------------*
026600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026700000801     C*----------------------------------------------------*
026800000801     C     DEFCAM        BEGSR
026900000801     C*
027000021113     C                   CLEAR                   FIVAB000
027100040802     C                   CLEAR                   FIVAT000
027200020619     C* Imposto i valori di default...
027300041026     C                   Z-ADD     0210420       VABCCM
027400041026     C                   Z-ADD     0210420       VATCCM
027500041026     C                   Z-ADD     021           VABLNP
027600041026     C                   Z-ADD     021           VATLNP
027700040714     C                   Z-ADD     000           VABCTR
027800040714     C                   MOVEL     '7Q'          VABCTM
027900040823     C                   MOVEL     '1'           VABCBO
028000020619     C* ... e poi verifico se sono stati passati come parametri
028100020619     C                   IF        vlrppt > *blanks
028200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028400020619     C                   EXSR      CHKNUM
028500020619     C                   IF        PiInt=*on
028600020619     C                   Z-ADD     PiVal         VABCCM
028700020619     C                   Z-ADD     PiVal         VATCCM
028800020619     C                   ENDIF
028900040506     C                   ENDIF
029000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029200020619     C                   EXSR      CHKNUM
029300020619     C                   IF        PiInt=*on
029400020619     C                   Z-ADD     PiVal         VABLNP
029500020619     C                   Z-ADD     PiVal         VATLNP
029600040506     C                   ENDIF
029700020619     C                   ENDIF
029800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
029900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030000020619     C                   EXSR      CHKNUM
030100020619     C                   IF        PiInt=*on
030200020619     C                   Z-ADD     PiVal         VABCTR
030300040506     C                   ENDIF
030400020619     C                   ENDIF
030500020619     C                   ENDIF
030600000801     C*
030700000801     C                   ENDSR
030800000801     C*----------------------------------------------------*
030900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031000000801     C*----------------------------------------------------*
031100040823     C     IMPVAB        BEGSR
031200040823     C*
031300020305     C                   EXSR      INZVAR
031400020305     C                   EXSR      DEFCAM
031500010305     C*
031600000801     C                   Z-ADD     *zeros        errore            1 0
031700000830     C                   MOVEL     datcor        VABAAS
031800020305     C                   MOVEL     datcor        VATAAS
031900040526     C                   MOVE      datcor        VABMGS
032000040823     C                   MOVE(P)   vlrpoi        VABFGS
032100040823     C                   MOVE(P)   vlrpoi        VATFGS
032200041026     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600041026     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
032700041026     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032800041026     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032900041026     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
033000041026     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
033100041026     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
033200041026     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
033300040506     C* CAD
033400041026     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033500010201     C                   EXSR      CHKNUM
033600010201     C                   IF        PiInt=*on
033700010201     C                   Z-ADD     PiVal         Num5_0
033800040506     C                   MOVEL(P)  Num5_0        VABCAD
033900010201     C                   ELSE
034000040506     C                   ADD       1             errore
034100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034200040506     C                             + ' ' + 'VABCAD'
034300010201     C                   ENDIF
034400040506     C* Reperisco la provincia dal CAP e dalla località
034500040526     C                   IF        VABCAD <> *blanks AND
034600040526     C                             VABPRD  = *blanks
034700040506     C                   CLEAR                   TISI95DS
034800040506     C                   EVAL      I95TCN = '3'
034900040506     C                   Z-ADD     datcor        I95DAT
035000040506     C                   EVAL      I95CAP = VABCAD
035100040506     C                   EVAL      I95LOC = VABLOD
035200040506     C                   CALL      'TISI95R'
035300040506     C                   PARM                    TISI95DS
035400040506     C                   EVAL      VABPRD = O95PRV
035500040506     C                   ENDIF
035600040506     C* NCL
035700041026     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035800040506     C                   EXSR      CHKNUM
035900040506     C                   IF        PiInt=*on
036000040506     C                   Z-ADD     PiVal         VABNCL
036100040506     C                   ELSE
036200040506     C                   ADD       1             errore
036300040506     C                   EVAL      vinmsg = %trimr(vinmsg)
036400040506     C                             + ' ' + 'VABNCL'
036500040506     C                   ENDIF
036600040506     C* PKB
036700041026     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036800010201     C                   EXSR      CHKNUM
036900010201     C                   IF        PiNum=*on
037000011115     C                   Z-ADD     PiVal         VABPKB
037100010201     C                   ELSE
037200010201     C                   ADD       1             errore
037300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037400010201     C                             + ' ' + 'VABPKB'
037500010201     C                   ENDIF
037600040506     C* NSP/RMN
037700041026      * negli ultimi due caratteri di questo campo c'è un '/P' li ho tolti
037800041026     C                   EVAL      PiStr=%trim(%subst(vindta:187:8))
037900010201     C                   EXSR      CHKNUM
038000010201     C                   IF        PiInt=*on
038100040506     C                   Z-ADD     PiVal         VABNSP
038200040506     C                   Z-ADD     PiVal         VATNSP
038300040506     C                   Z-ADD     PiVal         VABRMN
038400010201     C                   ELSE
038500010201     C                   ADD       1             errore
038600010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038700040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038800040526     C                   ENDIF
038900041026     C* VLB
039000041026     C                   EVAL      PiStr=%trim(%subst(vindta:296:7))
039100041026     C                   EXSR      CHKNUM
039200041026     C                   IF        PiNum=*on
039300041026     C                   Z-ADD     PiVal         VABVLB
039400041026     C                   ELSE
039500041026     C                   ADD       1             errore
039600041026     C                   EVAL      vinmsg = %trimr(vinmsg)
039700041026     C                             + ' ' + 'VABVLB'
039800041026     C                   ENDIF
039900041026     C* CAS/VCA
040000041026     C                   EVAL      PiStr=%trim(%subst(vindta:277:9))
040100041026     C                   EXSR      CHKNUM
040200041026     C                   IF        PiNum=*on
040300041026     C                   EVAL      VABCAS = PiVal / 100                         * gestisco 2 dec.
040400041026     c                   If        vabcas > *Zeros
040500041026     C                   MOVEL     '1'           FlgCAS
040600041026     C                   EVAL      VABVCA = 'EUR'
040700041026     c                   EndIf
040800041026     C                   ELSE
040900041026     C                   SETON                                        32
041000041026     C                   Z-ADD     *zeros        VABCAS
041100041026     C                   EVAL      vinmsg = %trimr(vinmsg)
041200041026     C                             + ' ' + 'VABCAS'
041300041026     C                   ENDIF
041400010205     C*
041500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041600040802     C                   IF        FlgCAS <> '0'
041700010205     C                   EVAL      VABCBO = '4'
041800010205     C                   ELSE
041900010205     C                   EVAL      VABCBO = '1'
042000010205     C                   ENDIF
042100041026     C* IAS/VAS
042200041026     C                   EVAL      PiStr=%trim(%subst(vindta:286:9))
042300041026     C                   EXSR      CHKNUM
042400041026     C                   IF        PiNum=*on
042500041026     C                   EVAL      VABIAS = PiVal / 100                         * gestisco 2 dec.
042600041026     c                   If        vabias > *Zeros
042700041026     C                   EVAL      VABVAS = 'EUR'
042800041026     c                   EndIf
042900041026     C                   ELSE
043000041026     C                   SETON                                        32
043100041026     C                   Z-ADD     *zeros        VABIAS
043200041026     C                   EVAL      vinmsg = %trimr(vinmsg)
043300041026     C                             + ' ' + 'VABIAS'
043400041026     C                   ENDIF
043500020305     C*
043600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043700011113     C                   EXSR      CHKIMPDIV
043800010202     C*
043900000801     C* Ebbene...
044000000801     C                   ADD       1             §CTRMO
044100010201     C                   IF        errore <> *zeros
044200000801     C                   ADD       1             §CTRNO
044300000801     C                   EVAL      recko = vindta
044400000801     C                   EXCEPT    rigadett
044500000801     C                   EVAL      vinflg = '2'
044600000801     C                   ELSE
044700010201     C                   ADD       1             §CTROKVB
044800000801     C                   ENDIF
044900000801     C*
045000000801     C                   ENDSR
045100010201     C*----------------------------------------------------*
045200040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
045300010201     C*----------------------------------------------------*
045400020305     C     WRIVAT        BEGSR
045500040802     C*
045600040802     C* Valorizzo i campi chiave
045700040802     C                   EVAL      VATNSP = VABNSP
045800040802     C                   EVAL      VATTRC = 'E'
045900040802     C*
046000040823     C* Valorizzo il barcode
046100041026     C                   EVAL      VATNOT = %subst(vindta:319:26)
046200040802     C                   WRITE     FIVAT000
046300041026
046400041026     c                   If        %subst(vindta:92:15) <> *Blanks
046500041026     C* Valorizzo i campi chiave
046600041026     C                   EVAL      VATNSP = VABNSP
046700041026     C                   EVAL      VATTRC = 'B'
046800041026     C*
046900041026     C* Valorizzo il barcode
047000041026     C                   EVAL      VATNOT = %subst(vindta:92:15)
047100041026     C                   WRITE     FIVAT000
047200041026     c                   EndIf
047300010201     C*
047400010201     C                   ENDSR
047500010202     C*----------------------------------------------------*
047600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047700010202     C*----------------------------------------------------*
047800020305     C     PREVAT        BEGSR
047900010202     C*
048000021113     C* Compongo il nome del membro da dare al FIVATWWR
048100010202     C                   eval      parmbr = vlrhdl
048200010202     C                   movel     'M'           parmbr
048300050317     C                   eval      parccm = '00210420'
048400010202     C                   eval      paropz = '1'
048500010202     C* Effettuo la chiamata al CLLE preposto
048600040506     C                   call(e)   'TITVVTC'
048700010202     C                   parm                    parccm
048800010202     C                   parm                    parmbr
048900010202     C                   parm                    paropz
049000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049100010202     C                   if        %error
049200010202     C                   movel     '1'           chkcall
049300010202     C                   else
049400010202     C                   movel     '0'           chkcall
049500010202     C                   endif
049600010202     C*
049700010202     C                   ENDSR
049800000801     C*----------------------------------------------------*
049900000801     C*  CONTROLLO NUMERICITA' CAMPI
050000000801     C*----------------------------------------------------*
050100000801     C     CHKNUM        BEGSR
050200000801     C*
050300000801     C                   call(e)   'ISNUMERIC'
050400000801     C                   PARM                    PiStr            30
050500041026     C                   PARM      '.'           PiDecChr          1
050600000801     C                   PARM      *ZEROS        PiVal            30 9
050700000801     C                   PARM      '0'           PiInt             1
050800000801     C                   PARM      '0'           PiNum             1
050900000801     C                   IF        %error
051000000801     C                   EVAL      PiInt=*off
051100000801     C                   ENDIF
051200000801     C*
051300000801     C                   ENDSR
051400000801     C***
051500000801
051600011113
051700011113     C*----------------------------------------------------*
051800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
051900011113     C*----------------------------------------------------*
052000011113     C     CHKIMPDIV     BEGSR
052100011113     C*
052200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052300011113     C                   Z-ADD     *zeros        wrkDec            9 9
052400011113     C*
052500011113     C* Come prima cosa effettuo considerazioni sulla divisa
052600011113     C                   IF        vabIAS > *zeros
052700011113     C                   IF        vabVAS <> 'EUR'
052800011113     C                   EVAL      vabVAS =  'ITL'
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   IF        vabCAS > *zeros
053300011113     C                   IF        vabVCA <> 'EUR'
053400011113     C                   EVAL      vabVCA =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C                   IF        vabVMD > *zeros
053900020305     C                   IF        vabVAD <> 'EUR'
054000011113     C                   EVAL      vabVAD =  'ITL'
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054500011113     C                   Z-ADD     vabIAS        wrkDec
054600011113     C                   IF        wrkDec > *zeros
054700011113     C                   IF        vabVAS = 'ITL'
054800011113     C                   EVAL      vabIAS = *zeros
054900011113     C                   ENDIF
055000011113     C                   ENDIF
055100011113     C*
055200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055300011113     C                   Z-ADD     vabCAS        wrkDec
055400011113     C                   IF        wrkDec > *zeros
055500011113     C                   IF        vabVCA = 'ITL'
055600011113     C                   EVAL      vabCAS = *zeros
055700011113     C                   ENDIF
055800011113     C                   ENDIF
055900011113     C*
056000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056100011113     C                   Z-ADD     vabVMD        wrkDec
056200011113     C                   IF        wrkDec > *zeros
056300011113     C                   IF        vabVAD = 'ITL'
056400011113     C                   EVAL      vabVMD = *zeros
056500011113     C                   ENDIF
056600011113     C                   ENDIF
056700011113     C*
056800011113     C                   ENDSR
056900011113     C***
057000011113
057100011113
057200000801
057300000801
057400990920      /TITLE Invio dei dati al punto operativo.
057500010202     C     invio         BEGSR
057600990920     C*
057700021113     C* 1° invio FIVAT
057800010201     C                   reset                   dscmz
057900010201     C                   move      vlrpoi        cmzdst
058000021113     C                   eval      cmzfld = 'FIVATWWR'
058100010201     C                   eval      cmzmbd = vlrhdl
058200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058300021009     C***                if        prmfir = *blanks
058400021113     C                   eval      cmzfla = 'FIVAT00F'
058500021113     C                   eval      cmzmba = 'FIVAT00F'
058600021009     C***                else
058700021009     C***                eval      cmzfla = prmfir
058800021009     C***                eval      cmzmba = prmfir
058900021009     C***                endif
059000010201     C                   eval      cmznrr = *zeros
059100020305     C                   move      §ctrokvt      cmznrr
059200021018     C                   eval      cmzlba = vlrfl1
059300010201     C                   call(e)   'TIS711C'
059400010201     C                   parm                    dscmz
059500010201     C                   parm      *blanks       esito
059600010205     C                   if        %error
059700010205     C                             or cmzerr = '1'
059800010205     C                             or esito  = '1'
059900010205     C                   eval      wrkesito = '3'
060000010205     C                   else
060100010201     C*
060200021113     C* 2° invio FIVAB
060300010201     C                   reset                   dscmz
060400010201     C                   move      vlrpoi        cmzdst
060500010201     C                   eval      cmzfld = vlrfou
060600010201     C                   eval      cmzmbd = vlrhdl
060700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060800021009     C***                if        prmfir = *blanks
060900021113     C                   eval      cmzfla = 'FIVAB00F'
061000021113     C                   eval      cmzmba = 'FIVAB00F'
061100021009     C***                else
061200021009     C***                eval      cmzfla = prmfir
061300021009     C***                eval      cmzmba = prmfir
061400021009     C***                endif
061500010201     C                   eval      cmznrr = *zeros
061600010201     C                   move      §ctrokvb      cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800010201     C                   call(e)   'TIS711C'
061900010201     C                   parm                    dscmz
062000010201     C                   parm      *blanks       esito
062100010201     C                   if        %error
062200010201     C                             or cmzerr = '1'
062300010201     C                             or esito  = '1'
062400010201     C                   eval      wrkesito = '3'
062500010201     C                   endif
062600010205     C                   endif
062700990920     C*
062800000613     C                   ENDSR
062900000613     C***
063000070411
063100070411     C     *pssr         BEGSR
063200070411     C*
063300070411     C                   if        %open(tivin00r)
063400070411     C                   close     tivin00r
063500070411     C                   endif
063600070411     C                   if        %open(fivabwwr)
063700070411     C                   close     fivabwwr
063800070411     C                   endif
063900070411     C                   if        %open(fivatwwr)
064000070411     C                   close     fivatwwr
064100070411     C                   endif
064200070411     C*
064300070411     C* Effettuo la chiamata al CLLE preposto
064400070411     C                   call(e)   'TITVVTC'
064500070411     C                   parm                    parccm
064600070411     C                   parm                    parmbr
064700070411     C                   parm      '2'           paropz
064800070411     C*
064900070411     C                   eval      wrkesito = '2'
065000070411     C*
065100070411     C                   seton                                        LR
065200070411     C*
065300070411     C                   ENDSR     '*CANCL'
065400070411     C***
065500070411
065600990910
065700000613     C     *inzsr        BEGSR
065800990910     C*
065900990910     C     *entry        plist
066000990920     C                   parm                    tivlrds
066100990921     C                   parm      wrkesito      esito
066200000724     C                   parm                    prmlit
066300000710     C                   parm                    prmfir
066400000613     C*
066500000830     C* CALCOLA LA DATA CORRENTE
066600000830     C                   time                    wn14             14 0
066700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066800000830     C                   z-add     wn8           g08dat
066900000830     C                   z-add     *zeros        g08inv
067000000830     C                   movel     '0'           g08err
067100000830     C                   call      'XSRDA8'
067200000830     C                   parm                    wlbda8
067300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067400000830     C*
067500000613     C                   ENDSR
067600000613     C***
067700990908
067800041026     Otitvf5p   E            riepilogo         2
067900990915     O                                              'Upload via Internet'
068000990915     O                                           +1 'Traduzione TIVIN00R -'
068100021113     O                                           +1 'FIVABWWR/FIVATWWR'
068200010201     O                                           +1 'Report testate bolle'
068300990915     O          E            riepilogo   2
068400990915     O                       wrkdata
068500990915     O                       wrkora              +1
068600990915     O                       procname            +1
068700990915     O          E            riepilogo   2
068800990915     O                                              'Cliente..................:'
068900990915     O                       VABCCM        z     +1
069000990915     O          E            riepilogo   2
069100990920     O                                              'Riferimento Strategi.....:'
069200990920     O                       vlrhdl              +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Giusti...................:'
069500010201     O                       §CTROKVB      2   +  1
069600990915     O          E            riepilogo   2
069700990915     O                                              'Sbagliati e corretti.....:'
069800971022     O                       §CTRMO        2   +  1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Sbagliati e scartati.....:'
070100971022     O                       §CTRNO        2   +  1
070200000613
070300041026     Otitvf5ps  E            testdett          2
070400000613     O                                              'Upload via Internet'
070500000613     O                                           +1 'Traduzione TIVIN00R -'
070600021113     O                                           +1 'FIVABWWR/FIVATWWR'
070700010201     O                                           +1 'Report testate bolle'
070800000616     O          E            testdett    3
070900000613     O                                           +2 'N° rec'
071000000613     O                                           +3 'Anteprima contenuto'
071100000616     O          E            rigadett    2
071200000613     O                       rrnum               +2
071300000621     O                       recko               +3
071400000616     O          E            findett     2
071500000613     O                       wrkdata
071600000613     O                       wrkora              +1
071700000613     O                       procname            +1
071800000616     O          E            findett     2
071900000613     O                                              'Cliente..................:'
072000000613     O                       VABCCM        z     +1
072100000616     O          E            findett     2
072200000613     O                                              'Riferimento Strategi.....:'
072300000613     O                       vlrhdl              +1
072400000616     O          E            findett     2
072500000613     O                                              'Giusti...................:'
072600010201     O                       §CTROKVB      2   +  1
072700000616     O          E            findett     2
072800000613     O                                              'Sbagliati e corretti.....:'
072900000613     O                       §CTRMO        2   +  1
073000000616     O          E            findett     2
073100000613     O                                              'Sbagliati e scartati.....:'
073200000613     O                       §CTRNO        2   +  1
073300000512** CMD - COMANDI CL
073400041026OVRPRTF FILE(TITVF5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
073500041026OVRPRTF FILE(TITVF5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073600041026DLTOVR FILE(TITVF5P TITVF5PS) LVL(*)
073700000512
073800000512
