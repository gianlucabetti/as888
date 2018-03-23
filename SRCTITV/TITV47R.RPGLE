000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030407     Ftitv47p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030407     Ftitv47ps  O    f  198        PRINTER usropn
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
002600030228     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600020305     D depspe          s             14    INZ(*blanks)
003700010213     D depdat          s              8  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400030107     D  Num7_0         s              7  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400000830     D*
005500990908
005600010201
005700010201
005800990915     C                   time                    wrkdata
005900990915     C                   time                    wrkora
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300000724     C*
006400000724     C* SE OCCORRE SPEDIRE IN FILIALE
006500021017     C                   if        vlrpoi <> *zeros
006600000724     C*
006700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006900021017     C     vlrpoi        chain     azorg01l
007000000724     C                   if        %found
007100000616     C                   movel(p)  CMD(1)        depcmd
007200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007300000616     C*
007400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007500011113     C                   Z-ADD     150           LENGH            15 5
007600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007700000616     C                   PARM                    depcmd
007800000616     C                   PARM                    LENGH
007900000724     C*
008000000724     C                   endif
008100000724     C                   endif
008200000616     C*
008300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008400000616     C                   movel(p)  CMD(2)        depcmd
008500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008600000616     C*
008700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008800011113     C                   Z-ADD     150           LENGH            15 5
008900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000000616     C                   PARM                    depcmd
009100000616     C                   PARM                    LENGH
009200000616     C*
009300030407     C                   if        not %open(titv47ps)
009400030407     C                   open      titv47ps
009500030408     C   50              except    testdett
009600000613     C                   endif
009700000613     C*
009800030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010000000613     C*
010100010202     C* Effettuo la chiamata al CLLE preposto
010200050128     C                   call(e)   'TITVVTC'
010300010202     C                   parm                    parccm
010400010202     C                   parm                    parmbr
010500010202     C                   parm      '2'           paropz
010600050201     C*
010700050201     C* Effettuo lancio TISI95 solo x chiusura
010800050201     C                   CLEAR                   TISI95DS
010900050201     C                   EVAL      I95TLA = 'C'
011000050201     C                   CALL      'TISI95R'
011100050201     C                   PARM                    TISI95DS
011200010202     C*
011300030407     C                   if        %open(titv47ps)
011400030408     C   50              except    findett
011500030407     C                   close     titv47ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400000801     C
012500010201     C                   seton                                        LR
012600000613
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300030407     C                   if        not %open(titv47p)
013400030407     C                   open      titv47p
013500990915     C                   endif
013600990915     C*
013700030408     C   50              except    riepilogo
013800990915     C*
013900030407     C                   if        %open(titv47p)
014000030407     C                   close     titv47p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600000801
014700910830     C*--------------------------------------------------------
014800030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014900910830     C*--------------------------------------------------------
015000030226     C     RWFILE        BEGSR
015100990910     C*
015200990914     C                   if        not %open(tivin00r)
015300990908     C                   open      tivin00r
015400990914     C                   endif
015500021017     C                   if        not %open(fivabwwr)
015600021017     C                   open      fivabwwr
015700990914     C                   endif
015800021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015900020305     C                   exsr      prevat
016000010201     C*
016100010202     C                   if        chkcall = '0'
016200010202     C*
016300021017     C                   if        not %open(fivatwwr)
016400021017     C                   open      fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   clear                   §CTROKVB          5 0
016800020305     C                   clear                   §CTROKVT          5 0
016900000801     C                   clear                   §CTRMO            5 0
017000000801     C                   clear                   §CTRNO            5 0
017100990910     C*
017200921023     C                   DO        *HIVAL
017300990913     C*
017400990915     C                   READ      tivin00r                               70
017500000905     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700000801     C*
017800000801     C                   if        *in70 = *off
017900000801     C                             and
018000000801     C                             (vinflg = *blanks
018100000801     C                              or vinflg = '0'
018200000801     C                              or vinflg = '2')
018300000801     C*
018400000801     C                   clear                   vinmsg
018500030407     C*
018600030407     C* Effettuo un primo test x verificare se trattasi di record da considerare
018700030407     C                   IF        %subst(vindta:1:4) <> *blanks
018800030407     C                   EVAL      PiStr=%trim(%subst(vindta:1:4))
018900030407     C                   EXSR      CHKNUM
019000030407     C*
019100030407     C                   IF        PiInt=*on
019200030226     C                   exsr      impvab
019300030226     C                   exsr      wrivab
019400030226     C                   exsr      impvat
019500030226     C                   exsr      wrivat
019600030407     C                   ENDIF
019700030407     C                   ENDIF
019800030407     C*
019900010305     C                   endif
020000000905     C*
020100000905     C                   else
020200000905     C                   eval      vinflg = '1'
020300000905     C                   endif
020400000905     C*
020500000905     C  N70              update    tivin000
020600000905     C*
020700991022     C  N70              ENDdo
020800010202     C*
020900010202     C                   endif
021000990910
021100990910     C* Se non ci sono record con errori ...
021200000710     C                   if        §ctrno = 0
021300990910     C* ... restituisco esito OK.
021400990921     C                   eval      wrkesito = '0'
021500990910     C                   else
021600010201     C                   if        §ctrokvb > 0
021700990921     C                   eval      wrkesito = '1'
021800000710     C                   else
021900000710     C                   eval      wrkesito = '2'
022000990910     C                   endif
022100000710     C                   endif
022200990910     C*
022300990914     C                   if        %open(tivin00r)
022400990908     C                   close     tivin00r
022500990914     C                   endif
022600021017     C                   if        %open(fivabwwr)
022700021017     C                   close     fivabwwr
022800990914     C                   endif
022900021017     C                   if        %open(fivatwwr)
023000021017     C                   close     fivatwwr
023100010201     C                   endif
023200021017     C*
023300021017     C                   if        vlrpoi <> 999
023400021017     C                   eval      vlrpoi = vlrpoi
023500021017     C                   endif
023600990910     C*
023700010201     C                   if        §ctrokvb > 0
023800021017     C                             and vlrpoi > *zeros
023900010202     C                   exsr      invio
024000990920     C                   endif
024100990920     C*
024200910830     C                   ENDSR
024300000613     C***
024400990920
024500000801     C*----------------------------------------------------*
024600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024700000801     C*----------------------------------------------------*
024800010201     C     INZVAR        BEGSR
024900000801     C*
025000010201     C                   Z-ADD     *zeros        Num5_0
025100030107     C                   Z-ADD     *zeros        Num7_0
025200030407     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
025300030407     C                   MOVEL     '0'           FlgCAS            1
025400000801     C*
025500000801     C                   ENDSR
025600000801     C*----------------------------------------------------*
025700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025800000801     C*----------------------------------------------------*
025900000801     C     DEFCAM        BEGSR
026000000801     C*
026100021017     C                   CLEAR                   FIVAB000
026200020619     C* Imposto i valori di default...
026300030408     C                   Z-ADD     0052454       VABCCM
026400030408     C                   Z-ADD     0052454       VATCCM
026500021017     C                   Z-ADD     vlrpoi        VABFGS
026600021017     C                   Z-ADD     vlrpoi        VATFGS
026700030408     C                   Z-ADD     005           VABLNP
026800030408     C                   Z-ADD     005           VATLNP
026900030408     C                   Z-ADD     200           VABCTR
027000021017     C                   MOVEL     '7Q'          VABCTM
027100020305     C                   MOVEL     'E'           VATTRC
027200020619     C* ... e poi verifico se sono stati passati come parametri
027300030408     C                   IF        %subst(vlrppt:1:7) <> *blanks
027400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027500020619     C                   EXSR      CHKNUM
027600020619     C                   IF        PiInt=*on
027700020619     C                   Z-ADD     PiVal         VABCCM
027800020619     C                   Z-ADD     PiVal         VATCCM
027900020619     C                   ENDIF
028000030408     C                   ENDIF
028100030408     C                   IF        %subst(vlrppt:8:3) <> *blanks
028200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
028300020619     C                   EXSR      CHKNUM
028400020619     C                   IF        PiInt=*on
028500020619     C                   Z-ADD     PiVal         VABLNP
028600020619     C                   Z-ADD     PiVal         VATLNP
028700020619     C                   ENDIF
028800030408     C                   ENDIF
028900030408     C                   IF        %subst(vlrppt:11:3) <> *blanks
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABCTR
029400020619     C                   ENDIF
029500030408     C                   ENDIF
029600030408     C                   IF        %subst(vlrppt:14:7) <> *blanks
029700030407     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:7))
029800030407     C                   EXSR      CHKNUM
029900030407     C                   IF        PiNum=*on
030000030407     C                   Z-ADD     PiVal         VABPKB
030100020619     C                   ENDIF
030200030407     C                   ENDIF
030300030408     C                   IF        %subst(vlrppt:15:1) ='S'
030400030408     C                   SETON                                        50
030500030408     C                   ELSE
030600030408     C                   SETOFF                                       50
030700030408     C                   ENDIF
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031200000801     C*----------------------------------------------------*
031300010201     C     IMPVAB        BEGSR
031400000801     C*
031500020305     C                   EXSR      INZVAR
031600020305     C                   EXSR      DEFCAM
031700010305     C*
031800030226     C                   Z-ADD     *zeros        errore            1 0
031900000830     C                   MOVEL     datcor        VABAAS
032000000830     C                   MOVE      datcor        VABMGS
032100030407     C* Reperisco i campi alfanumerici
032200030407     C                   EVAL      VABRSD=%trim(%subst(vindta:51:32))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600030620     C                   EVAL      VABRD2=%trim(%subst(vindta:213:35))+' cel.'+
032700030620     C                                    %trim(%subst(vindta:248:13))
032800030704     C                   IF        %trim(VABRD2) = 'cel.'
032900030704     C                   EVAL      VABRD2 = *blanks
033000030704     C                   ENDIF
033100030407     C                   EVAL      VABIND=%trim(%subst(vindta:83:32))
033200030407     C                   EVAL      VABLOD=%trim(%subst(vindta:137:30))
033300030407     C                   EVAL      VABRMA=%trim(%subst(vindta:26:10))
033400030620     C* Gestisco i campi relativi alla destinazione alternativa
033500030620     C                   IF        %len(%trim(%subst(vindta:261:35))) > 0
033600030620     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
033700030620     C                   ENDIF
033800030620     C                   IF        %len(%trim(%subst(vindta:296:35))) > 0
033900030620     C                   EVAL      VABIND=%trim(%subst(vindta:296:35))
034000030620     C                   ENDIF
034100030620     C                   IF        %len(%trim(%subst(vindta:336:2))) > 0
034200030620     C                   EVAL      VABPRD=%trim(%subst(vindta:336:2))
034300030620     C                   ENDIF
034400030620     C*
034500040223     C                   EVAL      VABRMO='tel.'+%trim(%subst(vindta:199:12))
034600030604     C                   MOVE      'AUTORIZZATO' VABNOT
034700030407     C                   EVAL      VABVCA='EUR'
034800030620     C* CAP/CAP alternativo
034900030620     C                   IF        %len(%trim(%subst(vindta:331:5))) > 0
035000030620     C                   EVAL      PiStr=%trim(%subst(vindta:331:5))
035100030620     C                   ELSE
035200030407     C                   EVAL      PiStr=%trim(%subst(vindta:115:5))
035300030620     C                   ENDIF
035400010201     C                   EXSR      CHKNUM
035500010201     C                   IF        PiInt=*on
035600010201     C                   Z-ADD     PiVal         Num5_0
035700021017     C                   MOVEL     Num5_0        VABCAD
035800010201     C                   ELSE
035900010201     C                   ADD       1             errore
036000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036100021017     C                             + ' ' + 'VABCAD'
036200010201     C                   ENDIF
036300030228     C* Reperisco la provincia dal CAP e dalla località
036400030228     C                   IF        VABCAD <> *blanks
036500030228     C                   CLEAR                   TISI95DS
036600030228     C                   EVAL      I95TCN = '3'
036700030228     C                   Z-ADD     datcor        I95DAT
036800030228     C                   EVAL      I95CAP = VABCAD
036900030228     C                   EVAL      I95LOC = VABLOD
037000030228     C                   CALL      'TISI95R'
037100030228     C                   PARM                    TISI95DS
037200030228     C                   EVAL      VABPRD = O95PRV
037300030228     C                   ENDIF
037400030226     C* NSP/RMN
037500030407     C                   EVAL      PiStr=%trim(%subst(vindta:6:10))
037600010201     C                   EXSR      CHKNUM
037700010201     C                   IF        PiInt=*on
037800021017     C                   Z-ADD     PiVal         VABRMN
037900030107     C                   Z-ADD     PiVal         Num7_0
038000030107     C                   MOVE(P)   Num7_0        VABNSP
038100030107     C                   MOVE(P)   Num7_0        VATNSP
038200010201     C                   ELSE
038300010201     C                   ADD       1             errore
038400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038600010201     C                   ENDIF
038700030226     C* NCL
038800030407     C                   Z-ADD     1             VABNCL
038900030407     C* PKB
039000030407     C                   IF        VABPKB = *zeros
039100030407     C                   Z-ADD     1             VABPKB
039200030107     C                   ENDIF
039300030407     C*
039400030407     C                   EVAL      PiStr=%trim(%subst(vindta:187:7))
039500030407     C* Detrmino se indipendentemente da giusto o sbagliato l'intenzione del cliente era d
039600030407     C* passare il contrassegno
039700030407     C                   IF        PiStr > *zeros
039800030407     C                   MOVEL     '1'           FlgCAS
039900030407     C*
040000030407     C                   EXSR      CHKNUM
040100030407     C                   IF        PiNum=*on
040200030714     C                   EVAL      VABCAS = PiVal/100
040300030422     C                   EVAL      VABTIC = 'BC'
040400030407     C                   ELSE
040500030407     C                   SETON                                        32
040600030407     C                   Z-ADD     *zeros        VABCAS
040700030407     C                   EVAL      vinmsg = %trimr(vinmsg)
040800030407     C                             + ' ' + 'VABCAS'
040900030407     C                   ENDIF
041000030407     C                   ENDIF
041100010205     C*
041200010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041300010205     C                   IF        VABCAS > *zeros
041400010205     C                   EVAL      VABCBO = '4'
041500010205     C                   ELSE
041600010205     C                   EVAL      VABCBO = '1'
041700010205     C                   ENDIF
041800020305     C*
041900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042000011113     C                   EXSR      CHKIMPDIV
042100010202     C*
042200000801     C* Ebbene...
042300000801     C                   ADD       1             §CTRMO
042400010201     C                   IF        errore <> *zeros
042500000801     C                   ADD       1             §CTRNO
042600000801     C                   EVAL      recko = vindta
042700030408     C   50              EXCEPT    rigadett
042800000801     C                   EVAL      vinflg = '2'
042900000801     C                   ELSE
043000010201     C                   ADD       1             §CTROKVB
043100000801     C                   ENDIF
043200000801     C*
043300000801     C                   ENDSR
043400030226     C*----------------------------------------------------*
043500030226     C*  SCARICAMENTO BUFFER RECORDS VAB
043600030226     C*----------------------------------------------------*
043700030226     C     WRIVAB        BEGSR
043800030226     C*
043900030226     C                   write     FIVAB000
044000030226     C*
044100030226     C                   ENDSR
044200030226     C*----------------------------------------------------*
044300030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
044400030226     C*----------------------------------------------------*
044500030226     C     IMPVAT        BEGSR
044600030226     C*
044700030226     C                   EXSR      INZVAR
044800030226     C                   EXSR      DEFCAM
044900030226     C*
045000030226     C                   Z-ADD     *zeros        errore            1 0
045100030226     C                   MOVEL     datcor        VATAAS
045200030226     C*
045300030506     C                   EVAL      VATNOT='0'+%subst(vindta:6:10)+                * n° ordine
045400030506     C                                        %subst(vindta:23:3)+                * cod. prodotto
045500030506     C                                        %subst(vindta:26:10)                * cod. cliente
045600030226     C*
045700030226     C* Ebbene...
045800030226     C                   ADD       1             §CTRMO
045900030226     C                   IF        errore <> *zeros
046000030226     C                   ADD       1             §CTRNO
046100030226     C                   EVAL      recko = vindta
046200030408     C   50              EXCEPT    rigadett
046300030226     C                   EVAL      vinflg = '2'
046400030226     C                   ELSE
046500030226     C                   ADD       1             §CTROKVB
046600030226     C                   ENDIF
046700030226     C*
046800030226     C                   ENDSR
046900010201     C*----------------------------------------------------*
047000021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047100010201     C*----------------------------------------------------*
047200020305     C     WRIVAT        BEGSR
047300010201     C*
047400021017     C                   write     FIVAT000
047500040223     C                   eval      vatTRC = 'B'
047600040223     C                   eval      vatNOT = %trim(%subst(vindta:199:12))
047700040223     C                   write     FIVAT000
047800010201     C*
047900010201     C                   ENDSR
048000010202     C*----------------------------------------------------*
048100021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048200010202     C*----------------------------------------------------*
048300020305     C     PREVAT        BEGSR
048400010202     C*
048500021017     C* Compongo il nome del membro da dare al FIVATWWR
048600010202     C                   eval      parmbr = vlrhdl
048700010202     C                   movel     'M'           parmbr
048800040223     C                   movel     *all'0'       parccm
048900040223     C                   move      vabccm        parccm
049000010202     C                   eval      paropz = '1'
049100010202     C* Effettuo la chiamata al CLLE preposto
049200050128     C                   call(e)   'TITVVTC'
049300010202     C                   parm                    parccm
049400010202     C                   parm                    parmbr
049500010202     C                   parm                    paropz
049600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049700010202     C                   if        %error
049800010202     C                   movel     '1'           chkcall
049900010202     C                   else
050000010202     C                   movel     '0'           chkcall
050100010202     C                   endif
050200010202     C*
050300010202     C                   ENDSR
050400000801     C*----------------------------------------------------*
050500000801     C*  CONTROLLO NUMERICITA' CAMPI
050600000801     C*----------------------------------------------------*
050700000801     C     CHKNUM        BEGSR
050800000801     C*
050900000801     C                   call(e)   'ISNUMERIC'
051000000801     C                   PARM                    PiStr            30
051100030107     C                   PARM      ','           PiDecChr          1
051200000801     C                   PARM      *ZEROS        PiVal            30 9
051300000801     C                   PARM      '0'           PiInt             1
051400000801     C                   PARM      '0'           PiNum             1
051500000801     C                   IF        %error
051600000801     C                   EVAL      PiInt=*off
051700030107     C                   EVAL      PiNum=*off
051800000801     C                   ENDIF
051900000801     C*
052000000801     C                   ENDSR
052100000801     C***
052200000801
052300011113
052400011113     C*----------------------------------------------------*
052500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052600011113     C*----------------------------------------------------*
052700011113     C     CHKIMPDIV     BEGSR
052800011113     C*
052900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053000011113     C                   Z-ADD     *zeros        wrkDec            9 9
053100011113     C*
053200011113     C* Come prima cosa effettuo considerazioni sulla divisa
053300011113     C                   IF        vabIAS > *zeros
053400011113     C                   IF        vabVAS <> 'EUR'
053500011113     C                   EVAL      vabVAS =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   IF        vabCAS > *zeros
054000011113     C                   IF        vabVCA <> 'EUR'
054100011113     C                   EVAL      vabVCA =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C                   IF        vabVMD > *zeros
054600020305     C                   IF        vabVAD <> 'EUR'
054700011113     C                   EVAL      vabVAD =  'ITL'
054800011113     C                   ENDIF
054900011113     C                   ENDIF
055000011113     C*
055100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055200011113     C                   Z-ADD     vabIAS        wrkDec
055300011113     C                   IF        wrkDec > *zeros
055400011113     C                   IF        vabVAS = 'ITL'
055500011113     C                   EVAL      vabIAS = *zeros
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056000011113     C                   Z-ADD     vabCAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVCA = 'ITL'
056300011113     C                   EVAL      vabCAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056800011113     C                   Z-ADD     vabVMD        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVAD = 'ITL'
057100011113     C                   EVAL      vabVMD = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C                   ENDSR
057600011113     C***
057700021017
057800000801
057900000801
058000990920      /TITLE Invio dei dati al punto operativo.
058100010202     C     invio         BEGSR
058200990920     C*
058300021017     C* 1° invio FIVAT
058400010201     C                   reset                   dscmz
058500021017     C                   move      vlrpoi        cmzdst
058600021017     C                   eval      cmzfld = 'FIVATWWR'
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000021017     C                   eval      cmzfla = 'FIVAT00F'
059100021017     C                   eval      cmzmba = 'FIVAT00F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700020305     C                   move      §ctrokvt      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010205     C                   if        %error
060300010205     C                             or cmzerr = '1'
060400010205     C                             or esito  = '1'
060500010205     C                   eval      wrkesito = '3'
060600010205     C                   else
060700010201     C*
060800021017     C* 2° invio FIVAB
060900010201     C                   reset                   dscmz
061000021017     C                   move      vlrpoi        cmzdst
061100010201     C                   eval      cmzfld = vlrfou
061200010201     C                   eval      cmzmbd = vlrhdl
061300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061400021009     C***                if        prmfir = *blanks
061500021017     C                   eval      cmzfla = 'FIVAB00F'
061600021017     C                   eval      cmzmba = 'FIVAB00F'
061700021009     C***                else
061800021009     C***                eval      cmzfla = prmfir
061900021009     C***                eval      cmzmba = prmfir
062000021009     C***                endif
062100010201     C                   eval      cmznrr = *zeros
062200010201     C                   move      §ctrokvb      cmznrr
062300021018     C                   eval      cmzlba = vlrfl1
062400010201     C                   call(e)   'TIS711C'
062500010201     C                   parm                    dscmz
062600010201     C                   parm      *blanks       esito
062700010201     C                   if        %error
062800010201     C                             or cmzerr = '1'
062900010201     C                             or esito  = '1'
063000010201     C                   eval      wrkesito = '3'
063100010201     C                   endif
063200010205     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600070411
063700070411     C     *pssr         BEGSR
063800070411     C*
063900070411     C                   if        %open(tivin00r)
064000070411     C                   close     tivin00r
064100070411     C                   endif
064200070411     C                   if        %open(fivabwwr)
064300070411     C                   close     fivabwwr
064400070411     C                   endif
064500070411     C                   if        %open(fivatwwr)
064600070411     C                   close     fivatwwr
064700070411     C                   endif
064800070411     C*
064900070411     C* Effettuo la chiamata al CLLE preposto
065000070411     C                   call(e)   'TITVVTC'
065100070411     C                   parm                    parccm
065200070411     C                   parm                    parmbr
065300070411     C                   parm      '2'           paropz
065400070411     C*
065500070411     C                   eval      wrkesito = '2'
065600070411     C*
065700070411     C                   seton                                        LR
065800070411     C*
065900070411     C                   ENDSR     '*CANCL'
066000070411     C***
066100070411
066200990910
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000000613     C*
067100000830     C* CALCOLA LA DATA CORRENTE
067200000830     C                   time                    wn14             14 0
067300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067400000830     C                   z-add     wn8           g08dat
067500000830     C                   z-add     *zeros        g08inv
067600000830     C                   movel     '0'           g08err
067700000830     C                   call      'XSRDA8'
067800000830     C                   parm                    wlbda8
067900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000830     C*
068100000613     C                   ENDSR
068200000613     C***
068300990908
068400030407     Otitv47p   E            riepilogo         2
068500990915     O                                              'Upload via Internet'
068600990915     O                                           +1 'Traduzione TIVIN00R -'
068700021017     O                                           +1 'FIVABWWR/FIVATWWR'
068800010201     O                                           +1 'Report testate bolle'
068900990915     O          E            riepilogo   2
069000990915     O                       wrkdata
069100990915     O                       wrkora              +1
069200990915     O                       procname            +1
069300990915     O          E            riepilogo   2
069400990915     O                                              'Cliente..................:'
069500990915     O                       VABCCM        z     +1
069600990915     O          E            riepilogo   2
069700990920     O                                              'Riferimento Strategi.....:'
069800990920     O                       vlrhdl              +1
069900990915     O          E            riepilogo   2
070000990915     O                                              'Giusti...................:'
070100010201     O                       §CTROKVB      2   +  1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Sbagliati e corretti.....:'
070400971022     O                       §CTRMO        2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e scartati.....:'
070700971022     O                       §CTRNO        2   +  1
070800000613
070900030407     Otitv47ps  E            testdett          2
071000000613     O                                              'Upload via Internet'
071100000613     O                                           +1 'Traduzione TIVIN00R -'
071200021017     O                                           +1 'FIVABWWR/FIVATWWR'
071300010201     O                                           +1 'Report testate bolle'
071400000616     O          E            testdett    3
071500000613     O                                           +2 'N° rec'
071600000613     O                                           +3 'Anteprima contenuto'
071700000616     O          E            rigadett    2
071800000613     O                       rrnum               +2
071900000621     O                       recko               +3
072000000616     O          E            findett     2
072100000613     O                       wrkdata
072200000613     O                       wrkora              +1
072300000613     O                       procname            +1
072400000616     O          E            findett     2
072500000613     O                                              'Cliente..................:'
072600000613     O                       VABCCM        z     +1
072700000616     O          E            findett     2
072800000613     O                                              'Riferimento Strategi.....:'
072900000613     O                       vlrhdl              +1
073000000616     O          E            findett     2
073100000613     O                                              'Giusti...................:'
073200010201     O                       §CTROKVB      2   +  1
073300000616     O          E            findett     2
073400000613     O                                              'Sbagliati e corretti.....:'
073500000613     O                       §CTRMO        2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e scartati.....:'
073800000613     O                       §CTRNO        2   +  1
073900000512** CMD - COMANDI CL
074000030407OVRPRTF FILE(TITV47P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
074100030407OVRPRTF FILE(TITV47PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074200030407DLTOVR FILE(TITV47P TITV47PS) LVL(*)
074300000512
074400000512
