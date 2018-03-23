000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030226     Ftitv36p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030226     Ftitv36ps  O    f  198        PRINTER usropn
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
003800020305     D curspe          s             14    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300010213
004400000801     D  Num5_0         s              5  0
004500030107     D  Num7_0         s              7  0
004600000830
004700000830     D*------------------
004800000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900000830     D*------------------
005000000830     D WLBDA8          DS                  INZ
005100000830     D  G08DAT                 1      8  0
005200000830     D  G08INV                 9     16  0
005300000830     D  G08ERR                17     17
005400000830     D  G08TGI                18     22  0
005500000830     D*
005600990908
005700010201
005800010201
005900990915     C                   time                    wrkdata
006000990915     C                   time                    wrkora
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400000724     C*
006500000724     C* SE OCCORRE SPEDIRE IN FILIALE
006600021017     C                   if        vlrpoi <> *zeros
006700000724     C*
006800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007000021017     C     vlrpoi        chain     azorg01l
007100000724     C                   if        %found
007200000616     C                   movel(p)  CMD(1)        depcmd
007300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007400000616     C*
007500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007600011113     C                   Z-ADD     150           LENGH            15 5
007700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007800000616     C                   PARM                    depcmd
007900000616     C                   PARM                    LENGH
008000000724     C*
008100000724     C                   endif
008200000724     C                   endif
008300000616     C*
008400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008500000616     C                   movel(p)  CMD(2)        depcmd
008600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008700000616     C*
008800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008900011113     C                   Z-ADD     150           LENGH            15 5
009000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009100000616     C                   PARM                    depcmd
009200000616     C                   PARM                    LENGH
009300000616     C*
009400030226     C                   if        not %open(titv36ps)
009500030226     C                   open      titv36ps
009600000616     C                   except    testdett
009700000613     C                   endif
009800000613     C*
009900030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010100000613     C*
010200010202     C* Effettuo la chiamata al CLLE preposto
010300050128     C                   call(e)   'TITVVTC'
010400010202     C                   parm                    parccm
010500010202     C                   parm                    parmbr
010600010202     C                   parm      '2'           paropz
010700050201     C*
010800050201     C* Effettuo lancio TISI95 solo x chiusura
010900050201     C                   CLEAR                   TISI95DS
011000050201     C                   EVAL      I95TLA = 'C'
011100050201     C                   CALL      'TISI95R'
011200050201     C                   PARM                    TISI95DS
011300010202     C*
011400030226     C                   if        %open(titv36ps)
011500000616     C                   except    findett
011600030226     C                   close     titv36ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500000801     C
012600010201     C                   seton                                        LR
012700000613
012800000613
012900000613     C*--------------------------------------------------------
013000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013100000613     C*--------------------------------------------------------
013200000621     C     STPR          BEGSR
013300000613     C*
013400030226     C                   if        not %open(titv36p)
013500030226     C                   open      titv36p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000030226     C                   if        %open(titv36p)
014100030226     C                   close     titv36p
014200990914     C                   endif
014300990910     C*
014400000613     C                   ENDSR
014500000613     C***
014600990908
014700000801
014800910830     C*--------------------------------------------------------
014900030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
015000910830     C*--------------------------------------------------------
015100030226     C     RWFILE        BEGSR
015200990910     C*
015300990914     C                   if        not %open(tivin00r)
015400990908     C                   open      tivin00r
015500990914     C                   endif
015600021017     C                   if        not %open(fivabwwr)
015700021017     C                   open      fivabwwr
015800990914     C                   endif
015900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000020305     C                   exsr      prevat
016100010201     C*
016200010202     C                   if        chkcall = '0'
016300010202     C*
016400021017     C                   if        not %open(fivatwwr)
016500021017     C                   open      fivatwwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   clear                   §CTROKVB          5 0
016900020305     C                   clear                   §CTROKVT          5 0
017000000801     C                   clear                   §CTRMO            5 0
017100000801     C                   clear                   §CTRNO            5 0
017200990910     C*
017300921023     C                   DO        *HIVAL
017400990913     C*
017500990915     C                   READ      tivin00r                               70
017600000905     C                   if        vindta > *blanks
017700000613     C                   add       1             rrnum
017800000801     C*
017900000801     C                   if        *in70 = *off
018000000801     C                             and
018100000801     C                             (vinflg = *blanks
018200000801     C                              or vinflg = '0'
018300000801     C                              or vinflg = '2')
018400000801     C*
018500000801     C                   clear                   vinmsg
018600030226     C*
018700030226     C* A seconda del tipo record (1=Testata 2=Dettaglio barcode) scrivo
018800030226     C* ora il FIVAB ora il FIVAT
018900030226     C                   if        %subst(vindta:1:1) = '1'
019000030226     C                   exsr      impvab
019100030226     C                   exsr      wrivab
019200030226     C                   endif
019300030226     C                   if        %subst(vindta:1:1) = '2'
019400030226     C                   exsr      impvat
019500030226     C                   exsr      wrivat
019600030226     C                   endif
019700010305     C                   endif
019800000905     C*
019900000905     C                   else
020000000905     C                   eval      vinflg = '1'
020100000905     C                   endif
020200000905     C*
020300000905     C  N70              update    tivin000
020400000905     C*
020500991022     C  N70              ENDdo
020600010202     C*
020700010202     C                   endif
020800990910
020900990910     C* Se non ci sono record con errori ...
021000000710     C                   if        §ctrno = 0
021100990910     C* ... restituisco esito OK.
021200990921     C                   eval      wrkesito = '0'
021300990910     C                   else
021400010201     C                   if        §ctrokvb > 0
021500990921     C                   eval      wrkesito = '1'
021600000710     C                   else
021700000710     C                   eval      wrkesito = '2'
021800990910     C                   endif
021900000710     C                   endif
022000990910     C*
022100990914     C                   if        %open(tivin00r)
022200990908     C                   close     tivin00r
022300990914     C                   endif
022400021017     C                   if        %open(fivabwwr)
022500021017     C                   close     fivabwwr
022600990914     C                   endif
022700021017     C                   if        %open(fivatwwr)
022800021017     C                   close     fivatwwr
022900010201     C                   endif
023000021017     C*
023100021017     C                   if        vlrpoi <> 999
023200021017     C                   eval      vlrpoi = vlrpoi
023300021017     C                   endif
023400990910     C*
023500010201     C                   if        §ctrokvb > 0
023600021017     C                             and vlrpoi > *zeros
023700010202     C                   exsr      invio
023800990920     C                   endif
023900990920     C*
024000910830     C                   ENDSR
024100000613     C***
024200990920
024300000801     C*----------------------------------------------------*
024400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024500000801     C*----------------------------------------------------*
024600010201     C     INZVAR        BEGSR
024700000801     C*
024800010201     C                   Z-ADD     *zeros        Num5_0
024900030107     C                   Z-ADD     *zeros        Num7_0
025000000801     C*
025100000801     C                   ENDSR
025200000801     C*----------------------------------------------------*
025300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025400000801     C*----------------------------------------------------*
025500000801     C     DEFCAM        BEGSR
025600000801     C*
025700021017     C                   CLEAR                   FIVAB000
025800020619     C* Imposto i valori di default...
025900030226     C                   Z-ADD     0502010       VABCCM
026000030226     C                   Z-ADD     0502010       VATCCM
026100021017     C                   Z-ADD     vlrpoi        VABFGS
026200021017     C                   Z-ADD     vlrpoi        VATFGS
026300030226     C                   Z-ADD     050           VABLNP
026400030226     C                   Z-ADD     050           VATLNP
026500080701     C                   Z-ADD     300           VABCTR
026600021017     C                   MOVEL     '7Q'          VABCTM
026700020305     C                   MOVEL     'E'           VATTRC
026800020619     C* ... e poi verifico se sono stati passati come parametri
026900020619     C                   IF        vlrppt > *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABCCM
027400020619     C                   Z-ADD     PiVal         VATCCM
027500020619     C                   ENDIF
027600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027700020619     C                   EXSR      CHKNUM
027800020619     C                   IF        PiInt=*on
027900020619     C                   Z-ADD     PiVal         VABLNP
028000020619     C                   Z-ADD     PiVal         VATLNP
028100020619     C                   ENDIF
028200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028300020619     C                   EXSR      CHKNUM
028400020619     C                   IF        PiInt=*on
028500020619     C                   Z-ADD     PiVal         VABCTR
028600020619     C                   ENDIF
028700020619     C                   ENDIF
028800000801     C*
028900000801     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100021017     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029200000801     C*----------------------------------------------------*
029300010201     C     IMPVAB        BEGSR
029400000801     C*
029500020305     C                   EXSR      INZVAR
029600020305     C                   EXSR      DEFCAM
029700010305     C*
029800030226     C                   Z-ADD     *zeros        errore            1 0
029900000830     C                   MOVEL     datcor        VABAAS
030000000830     C                   MOVE      datcor        VABMGS
030100030226     C*
030200030226     C                   EVAL      VABRSD=%trim(%subst(vindta:450:30))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600030226     C                   EVAL      VABIND=%trim(%subst(vindta:480:60))
030700030226     C                   EVAL      VABLOD=%trim(%subst(vindta:549:30))
030800030227     C                   EVAL      VABRMA=%trim(%subst(vindta:2:16))
030900030226     C                   EVAL      VABNOT=%trim(%subst(vindta:209:35))
031000030226     C                   EVAL      VABNT2=%trim(%subst(vindta:244:55))
031100030226     C* CAP
031200030226     C                   EVAL      PiStr=%trim(%subst(vindta:540:9))
031300010201     C                   EXSR      CHKNUM
031400010201     C                   IF        PiInt=*on
031500010201     C                   Z-ADD     PiVal         Num5_0
031600021017     C                   MOVEL     Num5_0        VABCAD
031700010201     C                   ELSE
031800010201     C                   ADD       1             errore
031900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032000021017     C                             + ' ' + 'VABCAD'
032100010201     C                   ENDIF
032200030228     C* Reperisco la provincia dal CAP e dalla località
032300030228     C                   IF        VABCAD <> *blanks
032400030228     C                   CLEAR                   TISI95DS
032500030228     C                   EVAL      I95TCN = '3'
032600030228     C                   Z-ADD     datcor        I95DAT
032700030228     C                   EVAL      I95CAP = VABCAD
032800030228     C                   EVAL      I95LOC = VABLOD
032900030228     C                   CALL      'TISI95R'
033000030228     C                   PARM                    TISI95DS
033100030228     C                   EVAL      VABPRD = O95PRV
033200030228     C                   ENDIF
033300030226     C* NSP/RMN
033400030226     C                   EVAL      PiStr=%trim(%subst(vindta:2:16))
033500010201     C                   EXSR      CHKNUM
033600010201     C                   IF        PiInt=*on
033700021017     C                   Z-ADD     PiVal         VABRMN
033800030107     C                   Z-ADD     PiVal         Num7_0
033900030107     C                   MOVE(P)   Num7_0        VABNSP
034000030107     C                   MOVE(P)   Num7_0        VATNSP
034100010201     C                   ELSE
034200010201     C                   ADD       1             errore
034300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034400021017     C                             + ' ' + 'VABNSP VATNSP VABRMN'
034500010201     C                   ENDIF
034600030226     C* NCL
034700030226     C                   EVAL      PiStr=%trim(%subst(vindta:99:5))
034800030107     C                   EXSR      CHKNUM
034900030107     C                   IF        PiInt=*on
035000030107     C                   Z-ADD     PiVal         VABNCL
035100030107     C                   ELSE
035200030107     C                   ADD       1             errore
035300030107     C                   EVAL      vinmsg = %trimr(vinmsg)
035400030107     C                             + ' ' + 'VABNCL'
035500030107     C                   ENDIF
035600030226     C* PKB
035700030226     C                   EVAL      PiStr=%trim(%subst(vindta:110:7))
035800030107     C                   EXSR      CHKNUM
035900030107     C                   IF        PiNum=*on
036000030226     C                   EVAL      PiVal = PiVal / 10                           *gestisco 1 decimale
036100030226     C                   Z-ADD(H)  PiVal         VABPKB
036200030107     C                   ELSE
036300030107     C                   ADD       1             errore
036400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036500030107     C                             + ' ' + 'VABPKB'
036600030107     C                   ENDIF
036700010205     C*
036800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
036900010205     C                   IF        VABCAS > *zeros
037000010205     C                   EVAL      VABCBO = '4'
037100010205     C                   ELSE
037200010205     C                   EVAL      VABCBO = '1'
037300010205     C                   ENDIF
037400020305     C*
037500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037600011113     C                   EXSR      CHKIMPDIV
037700010202     C*
037800000801     C* Ebbene...
037900000801     C                   ADD       1             §CTRMO
038000010201     C                   IF        errore <> *zeros
038100000801     C                   ADD       1             §CTRNO
038200000801     C                   EVAL      recko = vindta
038300000801     C                   EXCEPT    rigadett
038400000801     C                   EVAL      vinflg = '2'
038500000801     C                   ELSE
038600010201     C                   ADD       1             §CTROKVB
038700000801     C                   ENDIF
038800000801     C*
038900000801     C                   ENDSR
039000030226     C*----------------------------------------------------*
039100030226     C*  SCARICAMENTO BUFFER RECORDS VAB
039200030226     C*----------------------------------------------------*
039300030226     C     WRIVAB        BEGSR
039400030226     C*
039500030226     C                   write     FIVAB000
039600030226     C*
039700030226     C                   ENDSR
039800030226     C*----------------------------------------------------*
039900030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
040000030226     C*----------------------------------------------------*
040100030226     C     IMPVAT        BEGSR
040200030226     C*
040300030226     C                   EXSR      INZVAR
040400030226     C                   EXSR      DEFCAM
040500030226     C*
040600030226     C                   Z-ADD     *zeros        errore            1 0
040700030226     C                   MOVEL     datcor        VATAAS
040800030226     C*
040900030226     C                   EVAL      VATNOT=%trim(%subst(vindta:18:31))
041000030226     C*
041100030226     C* Ebbene...
041200030226     C                   ADD       1             §CTRMO
041300030226     C                   IF        errore <> *zeros
041400030226     C                   ADD       1             §CTRNO
041500030226     C                   EVAL      recko = vindta
041600030226     C                   EXCEPT    rigadett
041700030226     C                   EVAL      vinflg = '2'
041800030226     C                   ELSE
041900030226     C                   ADD       1             §CTROKVB
042000030226     C                   ENDIF
042100030226     C*
042200030226     C                   ENDSR
042300010201     C*----------------------------------------------------*
042400021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042500010201     C*----------------------------------------------------*
042600020305     C     WRIVAT        BEGSR
042700010201     C*
042800021017     C                   write     FIVAT000
042900010201     C*
043000010201     C                   ENDSR
043100010202     C*----------------------------------------------------*
043200021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043300010202     C*----------------------------------------------------*
043400020305     C     PREVAT        BEGSR
043500010202     C*
043600021017     C* Compongo il nome del membro da dare al FIVATWWR
043700010202     C                   eval      parmbr = vlrhdl
043800010202     C                   movel     'M'           parmbr
043900030226     C                   eval      parccm = '00502010'
044000010202     C                   eval      paropz = '1'
044100010202     C* Effettuo la chiamata al CLLE preposto
044200050128     C                   call(e)   'TITVVTC'
044300010202     C                   parm                    parccm
044400010202     C                   parm                    parmbr
044500010202     C                   parm                    paropz
044600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044700010202     C                   if        %error
044800010202     C                   movel     '1'           chkcall
044900010202     C                   else
045000010202     C                   movel     '0'           chkcall
045100010202     C                   endif
045200010202     C*
045300010202     C                   ENDSR
045400000801     C*----------------------------------------------------*
045500000801     C*  CONTROLLO NUMERICITA' CAMPI
045600000801     C*----------------------------------------------------*
045700000801     C     CHKNUM        BEGSR
045800000801     C*
045900000801     C                   call(e)   'ISNUMERIC'
046000000801     C                   PARM                    PiStr            30
046100030107     C                   PARM      ','           PiDecChr          1
046200000801     C                   PARM      *ZEROS        PiVal            30 9
046300000801     C                   PARM      '0'           PiInt             1
046400000801     C                   PARM      '0'           PiNum             1
046500000801     C                   IF        %error
046600000801     C                   EVAL      PiInt=*off
046700030107     C                   EVAL      PiNum=*off
046800000801     C                   ENDIF
046900000801     C*
047000000801     C                   ENDSR
047100000801     C***
047200000801
047300011113
047400011113     C*----------------------------------------------------*
047500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047600011113     C*----------------------------------------------------*
047700011113     C     CHKIMPDIV     BEGSR
047800011113     C*
047900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048000011113     C                   Z-ADD     *zeros        wrkDec            9 9
048100011113     C*
048200011113     C* Come prima cosa effettuo considerazioni sulla divisa
048300011113     C                   IF        vabIAS > *zeros
048400011113     C                   IF        vabVAS <> 'EUR'
048500011113     C                   EVAL      vabVAS =  'ITL'
048600011113     C                   ENDIF
048700011113     C                   ENDIF
048800011113     C*
048900011113     C                   IF        vabCAS > *zeros
049000011113     C                   IF        vabVCA <> 'EUR'
049100011113     C                   EVAL      vabVCA =  'ITL'
049200011113     C                   ENDIF
049300011113     C                   ENDIF
049400011113     C*
049500011113     C                   IF        vabVMD > *zeros
049600020305     C                   IF        vabVAD <> 'EUR'
049700011113     C                   EVAL      vabVAD =  'ITL'
049800011113     C                   ENDIF
049900011113     C                   ENDIF
050000011113     C*
050100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050200011113     C                   Z-ADD     vabIAS        wrkDec
050300011113     C                   IF        wrkDec > *zeros
050400011113     C                   IF        vabVAS = 'ITL'
050500011113     C                   EVAL      vabIAS = *zeros
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051000011113     C                   Z-ADD     vabCAS        wrkDec
051100011113     C                   IF        wrkDec > *zeros
051200011113     C                   IF        vabVCA = 'ITL'
051300011113     C                   EVAL      vabCAS = *zeros
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051800011113     C                   Z-ADD     vabVMD        wrkDec
051900011113     C                   IF        wrkDec > *zeros
052000011113     C                   IF        vabVAD = 'ITL'
052100011113     C                   EVAL      vabVMD = *zeros
052200011113     C                   ENDIF
052300011113     C                   ENDIF
052400011113     C*
052500011113     C                   ENDSR
052600011113     C***
052700021017
052800000801
052900000801
053000990920      /TITLE Invio dei dati al punto operativo.
053100010202     C     invio         BEGSR
053200990920     C*
053300021017     C* 1° invio FIVAT
053400010201     C                   reset                   dscmz
053500021017     C                   move      vlrpoi        cmzdst
053600021017     C                   eval      cmzfld = 'FIVATWWR'
053700010201     C                   eval      cmzmbd = vlrhdl
053800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053900021009     C***                if        prmfir = *blanks
054000021017     C                   eval      cmzfla = 'FIVAT00F'
054100021017     C                   eval      cmzmba = 'FIVAT00F'
054200021009     C***                else
054300021009     C***                eval      cmzfla = prmfir
054400021009     C***                eval      cmzmba = prmfir
054500021009     C***                endif
054600010201     C                   eval      cmznrr = *zeros
054700020305     C                   move      §ctrokvt      cmznrr
054800021018     C                   eval      cmzlba = vlrfl1
054900010201     C                   call(e)   'TIS711C'
055000010201     C                   parm                    dscmz
055100010201     C                   parm      *blanks       esito
055200010205     C                   if        %error
055300010205     C                             or cmzerr = '1'
055400010205     C                             or esito  = '1'
055500010205     C                   eval      wrkesito = '3'
055600010205     C                   else
055700010201     C*
055800021017     C* 2° invio FIVAB
055900010201     C                   reset                   dscmz
056000021017     C                   move      vlrpoi        cmzdst
056100010201     C                   eval      cmzfld = vlrfou
056200010201     C                   eval      cmzmbd = vlrhdl
056300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056400021009     C***                if        prmfir = *blanks
056500021017     C                   eval      cmzfla = 'FIVAB00F'
056600021017     C                   eval      cmzmba = 'FIVAB00F'
056700021009     C***                else
056800021009     C***                eval      cmzfla = prmfir
056900021009     C***                eval      cmzmba = prmfir
057000021009     C***                endif
057100010201     C                   eval      cmznrr = *zeros
057200010201     C                   move      §ctrokvb      cmznrr
057300021018     C                   eval      cmzlba = vlrfl1
057400010201     C                   call(e)   'TIS711C'
057500010201     C                   parm                    dscmz
057600010201     C                   parm      *blanks       esito
057700010201     C                   if        %error
057800010201     C                             or cmzerr = '1'
057900010201     C                             or esito  = '1'
058000010201     C                   eval      wrkesito = '3'
058100010201     C                   endif
058200010205     C                   endif
058300990920     C*
058400000613     C                   ENDSR
058500000613     C***
058600070411
058700070411     C     *pssr         BEGSR
058800070411     C*
058900070411     C                   if        %open(tivin00r)
059000070411     C                   close     tivin00r
059100070411     C                   endif
059200070411     C                   if        %open(fivabwwr)
059300070411     C                   close     fivabwwr
059400070411     C                   endif
059500070411     C                   if        %open(fivatwwr)
059600070411     C                   close     fivatwwr
059700070411     C                   endif
059800070411     C*
059900070411     C* Effettuo la chiamata al CLLE preposto
060000070411     C                   call(e)   'TITVVTC'
060100070411     C                   parm                    parccm
060200070411     C                   parm                    parmbr
060300070411     C                   parm      '2'           paropz
060400070411     C*
060500070411     C                   eval      wrkesito = '2'
060600070411     C*
060700070411     C                   seton                                        LR
060800070411     C*
060900070411     C                   ENDSR     '*CANCL'
061000070411     C***
061100070411
061200990910
061300000613     C     *inzsr        BEGSR
061400990910     C*
061500990910     C     *entry        plist
061600990920     C                   parm                    tivlrds
061700990921     C                   parm      wrkesito      esito
061800000724     C                   parm                    prmlit
061900000710     C                   parm                    prmfir
062000000613     C*
062100000830     C* CALCOLA LA DATA CORRENTE
062200000830     C                   time                    wn14             14 0
062300000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062400000830     C                   z-add     wn8           g08dat
062500000830     C                   z-add     *zeros        g08inv
062600000830     C                   movel     '0'           g08err
062700000830     C                   call      'XSRDA8'
062800000830     C                   parm                    wlbda8
062900000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063000000830     C*
063100000613     C                   ENDSR
063200000613     C***
063300990908
063400030226     Otitv36p   E            riepilogo         2
063500990915     O                                              'Upload via Internet'
063600990915     O                                           +1 'Traduzione TIVIN00R -'
063700021017     O                                           +1 'FIVABWWR/FIVATWWR'
063800010201     O                                           +1 'Report testate bolle'
063900990915     O          E            riepilogo   2
064000990915     O                       wrkdata
064100990915     O                       wrkora              +1
064200990915     O                       procname            +1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Cliente..................:'
064500990915     O                       VABCCM        z     +1
064600990915     O          E            riepilogo   2
064700990920     O                                              'Riferimento Strategi.....:'
064800990920     O                       vlrhdl              +1
064900990915     O          E            riepilogo   2
065000990915     O                                              'Giusti...................:'
065100010201     O                       §CTROKVB      2   +  1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Sbagliati e corretti.....:'
065400971022     O                       §CTRMO        2   +  1
065500990915     O          E            riepilogo   2
065600990915     O                                              'Sbagliati e scartati.....:'
065700971022     O                       §CTRNO        2   +  1
065800000613
065900030226     Otitv36ps  E            testdett          2
066000000613     O                                              'Upload via Internet'
066100000613     O                                           +1 'Traduzione TIVIN00R -'
066200021017     O                                           +1 'FIVABWWR/FIVATWWR'
066300010201     O                                           +1 'Report testate bolle'
066400000616     O          E            testdett    3
066500000613     O                                           +2 'N° rec'
066600000613     O                                           +3 'Anteprima contenuto'
066700000616     O          E            rigadett    2
066800000613     O                       rrnum               +2
066900000621     O                       recko               +3
067000000616     O          E            findett     2
067100000613     O                       wrkdata
067200000613     O                       wrkora              +1
067300000613     O                       procname            +1
067400000616     O          E            findett     2
067500000613     O                                              'Cliente..................:'
067600000613     O                       VABCCM        z     +1
067700000616     O          E            findett     2
067800000613     O                                              'Riferimento Strategi.....:'
067900000613     O                       vlrhdl              +1
068000000616     O          E            findett     2
068100000613     O                                              'Giusti...................:'
068200010201     O                       §CTROKVB      2   +  1
068300000616     O          E            findett     2
068400000613     O                                              'Sbagliati e corretti.....:'
068500000613     O                       §CTRMO        2   +  1
068600000616     O          E            findett     2
068700000613     O                                              'Sbagliati e scartati.....:'
068800000613     O                       §CTRNO        2   +  1
068900000512** CMD - COMANDI CL
069000030226OVRPRTF FILE(TITV36P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069100030226OVRPRTF FILE(TITV36PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
069200030226DLTOVR FILE(TITV36P TITV36PS) LVL(*)
069300000512
069400000512
