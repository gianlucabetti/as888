000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040714     Ftitvd6p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040714     Ftitvd6ps  O    f  198        PRINTER usropn
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
003600040526     D depspe          s             10    INZ(*blanks)
003700040526     D curspe          s             10    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400990908
005500010201
005600010201
005700990915     C                   time                    wrkdata
005800990915     C                   time                    wrkora
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200000724     C*
006300000724     C* SE OCCORRE SPEDIRE IN FILIALE
006400000724     C                   if        vlrpoi <> *zeros
006500000724     C*
006600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006800000724     C     vlrpoi        chain     azorg01l
006900000724     C                   if        %found
007000000616     C                   movel(p)  CMD(1)        depcmd
007100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007200000616     C*
007300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007400011113     C                   Z-ADD     150           LENGH            15 5
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000616     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800000724     C*
007900000724     C                   endif
008000000724     C                   endif
008100000616     C*
008200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008300000616     C                   movel(p)  CMD(2)        depcmd
008400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008500000616     C*
008600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008700011113     C                   Z-ADD     150           LENGH            15 5
008800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008900000616     C                   PARM                    depcmd
009000000616     C                   PARM                    LENGH
009100000616     C*
009200040714     C                   if        not %open(titvd6ps)
009300040714     C                   open      titvd6ps
009400000616     C                   except    testdett
009500000613     C                   endif
009600000613     C*
009700040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900000613     C*
010000010202     C* Effettuo la chiamata al CLLE preposto
010100040506     C                   call(e)   'TITVVTC'
010200010202     C                   parm                    parccm
010300010202     C                   parm                    parmbr
010400010202     C                   parm      '2'           paropz
010500050201     C*
010600050201     C* Effettuo lancio TISI95 solo x chiusura
010700050201     C                   CLEAR                   TISI95DS
010800050201     C                   EVAL      I95TLA = 'C'
010900050201     C                   CALL      'TISI95R'
011000050201     C                   PARM                    TISI95DS
011100010202     C*
011200040714     C                   if        %open(titvd6ps)
011300000616     C                   except    findett
011400040714     C                   close     titvd6ps
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
013200040714     C                   if        not %open(titvd6p)
013300040714     C                   open      titvd6p
013400990915     C                   endif
013500990915     C*
013600990915     C                   except    riepilogo
013700990915     C*
013800040714     C                   if        %open(titvd6p)
013900040714     C                   close     titvd6p
014000990914     C                   endif
014100990910     C*
014200000613     C                   ENDSR
014300000613     C***
014400990908
014500000801
014600910830     C*--------------------------------------------------------
014700040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014800910830     C*--------------------------------------------------------
014900040526     C     RWFILE        BEGSR
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
018700040526     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
018800020305     C                   MOVEL(p)  PiStr         curspe
018900010305     C*
019000010305     C                   if        depspe = *blanks                             => 1° giro
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200020305     C                   exsr      impvab
019300020305     C                   exsr      wrivat                                       => carico VAT
019400010305     C                   else
019500010305     C                   if        curspe <> depspe                             => rottura di spediz
019600010305     C                   eval      depspe = curspe                              => memorizz. spediz
019700020305     C                   exsr      wrivab
019800020305     C                   exsr      impvab
019900020305     C                   exsr      wrivat                                       => carico VAT
020000020305     C                   else                                                   => x stessa spediz
020100040526     C***                exsr      impvab
020200040726     C                   exsr      wrivat                                       => carico VAT
020300010305     C                   endif
020400010305     C                   endif
020500010305     C                   endif
020600000905     C*
020700000905     C                   else
020800000905     C                   eval      vinflg = '1'
020900000905     C                   endif
021000000905     C*
021100000905     C  N70              update    tivin000
021200000905     C*
021300991022     C  N70              ENDdo
021400010202     C*
021500020305     C* Scarico i VAB rimasti "in sospeso"
021600020305     C                   exsr      wrivab
021700010202     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500021113     C                   if        %open(fivabwwr)
023600021113     C                   close     fivabwwr
023700990914     C                   endif
023800021113     C                   if        %open(fivatwwr)
023900021113     C                   close     fivatwwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900010305
025000010305     C*----------------------------------------------------*
025100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025200010305     C*----------------------------------------------------*
025300020305     C     WRIVAB        BEGSR
025400010305     C*
025500021113     C                   write     fivab000                                     => scarico il VAB
025600010305     C*
025700010305     C                   ENDSR
025800990920
025900000801     C*----------------------------------------------------*
026000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026100000801     C*----------------------------------------------------*
026200010201     C     INZVAR        BEGSR
026300000801     C*
026400010201     C                   Z-ADD     *zeros        Num5_0
026500000801     C*
026600000801     C                   ENDSR
026700000801     C*----------------------------------------------------*
026800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026900000801     C*----------------------------------------------------*
027000000801     C     DEFCAM        BEGSR
027100000801     C*
027200021113     C                   CLEAR                   FIVAB000
027300020619     C* Imposto i valori di default...
027400040714     C                   Z-ADD     0012223       VABCCM
027500040714     C                   Z-ADD     0012223       VATCCM
027600040714     C                   Z-ADD     001           VABLNP
027700040714     C                   Z-ADD     001           VATLNP
027800040714     C                   Z-ADD     000           VABCTR
027900040714     C                   MOVEL     '7Q'          VABCTM
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
031100010201     C     IMPVAB        BEGSR
031200000801     C*
031300020305     C                   EXSR      INZVAR
031400020305     C                   EXSR      DEFCAM
031500010305     C*
031600000801     C                   Z-ADD     *zeros        errore            1 0
031700000830     C                   MOVEL     datcor        VABAAS
031800020305     C                   MOVEL     datcor        VATAAS
031900040526     C                   MOVE      datcor        VABMGS
032000021113     C                   MOVE(P)   vlrpoi        VABFGS
032100021113     C                   MOVE(P)   vlrpoi        VATFGS
032200040714     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600040714     C                   EVAL      VABIND=%trim(%subst(vindta:57:50))
032700040526     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
032800040526     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
032900040714     C                   EVAL      VABRMA=%trim(%subst(vindta:472:20))
033000040506     C* CAD
033100040526     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiInt=*on
033400010201     C                   Z-ADD     PiVal         Num5_0
033500040506     C                   MOVEL(P)  Num5_0        VABCAD
033600010201     C                   ELSE
033700040506     C                   ADD       1             errore
033800010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033900040506     C                             + ' ' + 'VABCAD'
034000010201     C                   ENDIF
034100040506     C* Reperisco la provincia dal CAP e dalla località
034200040526     C                   IF        VABCAD <> *blanks AND
034300040526     C                             VABPRD  = *blanks
034400040506     C                   CLEAR                   TISI95DS
034500040506     C                   EVAL      I95TCN = '3'
034600040506     C                   Z-ADD     datcor        I95DAT
034700040506     C                   EVAL      I95CAP = VABCAD
034800040506     C                   EVAL      I95LOC = VABLOD
034900040506     C                   CALL      'TISI95R'
035000040506     C                   PARM                    TISI95DS
035100040506     C                   EVAL      VABPRD = O95PRV
035200040506     C                   ENDIF
035300040506     C* NCL
035400040526     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035500040506     C                   EXSR      CHKNUM
035600040506     C                   IF        PiInt=*on
035700040506     C                   Z-ADD     PiVal         VABNCL
035800040506     C                   ELSE
035900040506     C                   ADD       1             errore
036000040506     C                   EVAL      vinmsg = %trimr(vinmsg)
036100040506     C                             + ' ' + 'VABNCL'
036200040506     C                   ENDIF
036300040506     C* PKB
036400040526     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036500010201     C                   EXSR      CHKNUM
036600010201     C                   IF        PiNum=*on
036700011115     C                   Z-ADD     PiVal         VABPKB
036800010201     C                   ELSE
036900010201     C                   ADD       1             errore
037000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037100010201     C                             + ' ' + 'VABPKB'
037200010201     C                   ENDIF
037300040506     C* NSP/RMN
037400040526     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037500010201     C                   EXSR      CHKNUM
037600010201     C                   IF        PiInt=*on
037700040506     C                   Z-ADD     PiVal         VABNSP
037800040506     C                   Z-ADD     PiVal         VATNSP
037900040506     C                   Z-ADD     PiVal         VABRMN
038000010201     C                   ELSE
038100010201     C                   ADD       1             errore
038200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038300040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038400040526     C                   ENDIF
038500040714     C* IAS
038600051126     C                   EVAL      PiStr=%trim(%subst(vindta:492:10))
038700051126     C                   IF        PiStr<>*blanks
038800040714     C                   EXSR      CHKNUM
038900040714     C                   IF        PiNum=*on
039000040714     C                   Z-ADD     PiVal         VABIAS
039100040714     C                   EVAL      VABVAS = 'EUR'
039200040714     C                   ELSE
039300040714     C                   ADD       1             errore
039400040714     C                   EVAL      vinmsg = %trimr(vinmsg)
039500040714     C                             + ' ' + 'VABIAS'
039600040714     C                   ENDIF
039700051126     C                   ENDIF
039800010205     C*
039900010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
040000010205     C                   IF        VABCAS > *zeros
040100010205     C                   EVAL      VABCBO = '4'
040200010205     C                   ELSE
040300010205     C                   EVAL      VABCBO = '1'
040400010205     C                   ENDIF
040500020305     C*
040600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040700011113     C                   EXSR      CHKIMPDIV
040800010202     C*
040900000801     C* Ebbene...
041000000801     C                   ADD       1             §CTRMO
041100010201     C                   IF        errore <> *zeros
041200000801     C                   ADD       1             §CTRNO
041300000801     C                   EVAL      recko = vindta
041400000801     C                   EXCEPT    rigadett
041500000801     C                   EVAL      vinflg = '2'
041600000801     C                   ELSE
041700010201     C                   ADD       1             §CTROKVB
041800000801     C                   ENDIF
041900000801     C*
042000000801     C                   ENDSR
042100010201     C*----------------------------------------------------*
042200040714     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042300010201     C*----------------------------------------------------*
042400020305     C     WRIVAT        BEGSR
042500010201     C*
042600021113     C* Valorizzo l buffer di scrittura del FIVAT
042700040714     C                   IF        %subst(vindta:412:30) <> *blanks  AND
042800040714     C                             %subst(vindta:412:30) <> 'NULL'
042900040714     C                   EVAL      VATTRC = 'A'
043000040714     C                   EVAL      VATNOT=%subst(vindta:412:30)
043100040526     C                   WRITE     FIVAT000
043200040506     C                   ENDIF
043300040714     C                   IF        %subst(vindta:442:30) <> *blanks AND
043400040714     C                             %subst(vindta:442:30) <> 'NULL'
043500040714     C                   EVAL      VATTRC = 'B'
043600040714     C                   EVAL      VATNOT=%subst(vindta:442:30)
043700040714     C                   WRITE     FIVAT000
043800040714     C                   ENDIF
043900051128     C                   IF        %subst(vindta:341:7) <> *blanks AND
044000051128     C                             %trim(%subst(vindta:341:7)) <> 'NULL'
044100040714     C                   EVAL      VATTRC = 'E'
044200051128     C                   EVAL      VATNOT=%subst(vindta:341:7)
044300040714     C                   WRITE     FIVAT000
044400040714     C                   ENDIF
044500010201     C*
044600010201     C                   ENDSR
044700010202     C*----------------------------------------------------*
044800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044900010202     C*----------------------------------------------------*
045000020305     C     PREVAT        BEGSR
045100010202     C*
045200021113     C* Compongo il nome del membro da dare al FIVATWWR
045300010202     C                   eval      parmbr = vlrhdl
045400010202     C                   movel     'M'           parmbr
045500040714     C                   eval      parccm = '00012223'
045600010202     C                   eval      paropz = '1'
045700010202     C* Effettuo la chiamata al CLLE preposto
045800040506     C                   call(e)   'TITVVTC'
045900010202     C                   parm                    parccm
046000010202     C                   parm                    parmbr
046100010202     C                   parm                    paropz
046200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300010202     C                   if        %error
046400010202     C                   movel     '1'           chkcall
046500010202     C                   else
046600010202     C                   movel     '0'           chkcall
046700010202     C                   endif
046800010202     C*
046900010202     C                   ENDSR
047000000801     C*----------------------------------------------------*
047100000801     C*  CONTROLLO NUMERICITA' CAMPI
047200000801     C*----------------------------------------------------*
047300000801     C     CHKNUM        BEGSR
047400000801     C*
047500000801     C                   call(e)   'ISNUMERIC'
047600000801     C                   PARM                    PiStr            30
047700040714     C                   PARM      ','           PiDecChr          1
047800000801     C                   PARM      *ZEROS        PiVal            30 9
047900000801     C                   PARM      '0'           PiInt             1
048000000801     C                   PARM      '0'           PiNum             1
048100000801     C                   IF        %error
048200000801     C                   EVAL      PiInt=*off
048300000801     C                   ENDIF
048400000801     C*
048500000801     C                   ENDSR
048600000801     C***
048700000801
048800011113
048900011113     C*----------------------------------------------------*
049000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049100011113     C*----------------------------------------------------*
049200011113     C     CHKIMPDIV     BEGSR
049300011113     C*
049400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049500011113     C                   Z-ADD     *zeros        wrkDec            9 9
049600011113     C*
049700011113     C* Come prima cosa effettuo considerazioni sulla divisa
049800011113     C                   IF        vabIAS > *zeros
049900011113     C                   IF        vabVAS <> 'EUR'
050000011113     C                   EVAL      vabVAS =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C                   IF        vabCAS > *zeros
050500011113     C                   IF        vabVCA <> 'EUR'
050600011113     C                   EVAL      vabVCA =  'ITL'
050700011113     C                   ENDIF
050800011113     C                   ENDIF
050900011113     C*
051000011113     C                   IF        vabVMD > *zeros
051100020305     C                   IF        vabVAD <> 'EUR'
051200011113     C                   EVAL      vabVAD =  'ITL'
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051700011113     C                   Z-ADD     vabIAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVAS = 'ITL'
052000011113     C                   EVAL      vabIAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
052500011113     C                   Z-ADD     vabCAS        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVCA = 'ITL'
052800011113     C                   EVAL      vabCAS = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053300011113     C                   Z-ADD     vabVMD        wrkDec
053400011113     C                   IF        wrkDec > *zeros
053500011113     C                   IF        vabVAD = 'ITL'
053600011113     C                   EVAL      vabVMD = *zeros
053700011113     C                   ENDIF
053800011113     C                   ENDIF
053900011113     C*
054000011113     C                   ENDSR
054100011113     C***
054200011113
054300011113
054400000801
054500000801
054600990920      /TITLE Invio dei dati al punto operativo.
054700010202     C     invio         BEGSR
054800990920     C*
054900021113     C* 1° invio FIVAT
055000010201     C                   reset                   dscmz
055100010201     C                   move      vlrpoi        cmzdst
055200021113     C                   eval      cmzfld = 'FIVATWWR'
055300010201     C                   eval      cmzmbd = vlrhdl
055400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055500021009     C***                if        prmfir = *blanks
055600021113     C                   eval      cmzfla = 'FIVAT00F'
055700021113     C                   eval      cmzmba = 'FIVAT00F'
055800021009     C***                else
055900021009     C***                eval      cmzfla = prmfir
056000021009     C***                eval      cmzmba = prmfir
056100021009     C***                endif
056200010201     C                   eval      cmznrr = *zeros
056300020305     C                   move      §ctrokvt      cmznrr
056400021018     C                   eval      cmzlba = vlrfl1
056500010201     C                   call(e)   'TIS711C'
056600010201     C                   parm                    dscmz
056700010201     C                   parm      *blanks       esito
056800010205     C                   if        %error
056900010205     C                             or cmzerr = '1'
057000010205     C                             or esito  = '1'
057100010205     C                   eval      wrkesito = '3'
057200010205     C                   else
057300010201     C*
057400021113     C* 2° invio FIVAB
057500010201     C                   reset                   dscmz
057600010201     C                   move      vlrpoi        cmzdst
057700010201     C                   eval      cmzfld = vlrfou
057800010201     C                   eval      cmzmbd = vlrhdl
057900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058000021009     C***                if        prmfir = *blanks
058100021113     C                   eval      cmzfla = 'FIVAB00F'
058200021113     C                   eval      cmzmba = 'FIVAB00F'
058300021009     C***                else
058400021009     C***                eval      cmzfla = prmfir
058500021009     C***                eval      cmzmba = prmfir
058600021009     C***                endif
058700010201     C                   eval      cmznrr = *zeros
058800010201     C                   move      §ctrokvb      cmznrr
058900021018     C                   eval      cmzlba = vlrfl1
059000010201     C                   call(e)   'TIS711C'
059100010201     C                   parm                    dscmz
059200010201     C                   parm      *blanks       esito
059300010201     C                   if        %error
059400010201     C                             or cmzerr = '1'
059500010201     C                             or esito  = '1'
059600010201     C                   eval      wrkesito = '3'
059700010201     C                   endif
059800010205     C                   endif
059900990920     C*
060000000613     C                   ENDSR
060100000613     C***
060200070411
060300070411     C     *pssr         BEGSR
060400070411     C*
060500070411     C                   if        %open(tivin00r)
060600070411     C                   close     tivin00r
060700070411     C                   endif
060800070411     C                   if        %open(fivabwwr)
060900070411     C                   close     fivabwwr
061000070411     C                   endif
061100070411     C                   if        %open(fivatwwr)
061200070411     C                   close     fivatwwr
061300070411     C                   endif
061400070411     C*
061500070411     C* Effettuo la chiamata al CLLE preposto
061600070411     C                   call(e)   'TITVVTC'
061700070411     C                   parm                    parccm
061800070411     C                   parm                    parmbr
061900070411     C                   parm      '2'           paropz
062000070411     C*
062100070411     C                   eval      wrkesito = '2'
062200070411     C*
062300070411     C                   seton                                        LR
062400070411     C*
062500070411     C                   ENDSR     '*CANCL'
062600070411     C***
062700990910
062800000613     C     *inzsr        BEGSR
062900990910     C*
063000990910     C     *entry        plist
063100990920     C                   parm                    tivlrds
063200990921     C                   parm      wrkesito      esito
063300000724     C                   parm                    prmlit
063400000710     C                   parm                    prmfir
063500000613     C*
063600000830     C* CALCOLA LA DATA CORRENTE
063700000830     C                   time                    wn14             14 0
063800000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063900000830     C                   z-add     wn8           g08dat
064000000830     C                   z-add     *zeros        g08inv
064100000830     C                   movel     '0'           g08err
064200000830     C                   call      'XSRDA8'
064300000830     C                   parm                    wlbda8
064400000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064500000830     C*
064600000613     C                   ENDSR
064700000613     C***
064800990908
064900040714     Otitvd6p   E            riepilogo         2
065000990915     O                                              'Upload via Internet'
065100990915     O                                           +1 'Traduzione TIVIN00R -'
065200021113     O                                           +1 'FIVABWWR/FIVATWWR'
065300010201     O                                           +1 'Report testate bolle'
065400990915     O          E            riepilogo   2
065500990915     O                       wrkdata
065600990915     O                       wrkora              +1
065700990915     O                       procname            +1
065800990915     O          E            riepilogo   2
065900990915     O                                              'Cliente..................:'
066000990915     O                       VABCCM        z     +1
066100990915     O          E            riepilogo   2
066200990920     O                                              'Riferimento Strategi.....:'
066300990920     O                       vlrhdl              +1
066400990915     O          E            riepilogo   2
066500990915     O                                              'Giusti...................:'
066600010201     O                       §CTROKVB      2   +  1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Sbagliati e corretti.....:'
066900971022     O                       §CTRMO        2   +  1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Sbagliati e scartati.....:'
067200971022     O                       §CTRNO        2   +  1
067300000613
067400040714     Otitvd6ps  E            testdett          2
067500000613     O                                              'Upload via Internet'
067600000613     O                                           +1 'Traduzione TIVIN00R -'
067700021113     O                                           +1 'FIVABWWR/FIVATWWR'
067800010201     O                                           +1 'Report testate bolle'
067900000616     O          E            testdett    3
068000000613     O                                           +2 'N° rec'
068100000613     O                                           +3 'Anteprima contenuto'
068200000616     O          E            rigadett    2
068300000613     O                       rrnum               +2
068400000621     O                       recko               +3
068500000616     O          E            findett     2
068600000613     O                       wrkdata
068700000613     O                       wrkora              +1
068800000613     O                       procname            +1
068900000616     O          E            findett     2
069000000613     O                                              'Cliente..................:'
069100000613     O                       VABCCM        z     +1
069200000616     O          E            findett     2
069300000613     O                                              'Riferimento Strategi.....:'
069400000613     O                       vlrhdl              +1
069500000616     O          E            findett     2
069600000613     O                                              'Giusti...................:'
069700010201     O                       §CTROKVB      2   +  1
069800000616     O          E            findett     2
069900000613     O                                              'Sbagliati e corretti.....:'
070000000613     O                       §CTRMO        2   +  1
070100000616     O          E            findett     2
070200000613     O                                              'Sbagliati e scartati.....:'
070300000613     O                       §CTRNO        2   +  1
070400000512** CMD - COMANDI CL
070500040714OVRPRTF FILE(TITVD6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070600040714OVRPRTF FILE(TITVD6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070700040714DLTOVR FILE(TITVD6P TITVD6PS) LVL(*)
070800000512
070900000512
