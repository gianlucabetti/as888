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
019200060919     C                   exsr      inzvar                                       => inizializzaz file
019300060919     C                   exsr      defcam                                       => impostaz. default
019400060919     C                   exsr      impvab                                       => carico VAB
019500060919     C                   exsr      wrivat                                       => scarico VAT
019600010305     C                   else
019700010305     C                   if        curspe <> depspe                             => rottura di spediz
019800010305     C                   eval      depspe = curspe                              => memorizz. spediz
019900060919     C                   exsr      wrivab                                       => scarico VAB
020000060919     C                   exsr      inzvar                                       => inizializzaz file
020100060919     C                   exsr      defcam                                       => impostaz. default
020200060919     C                   exsr      impvab                                       => carico VAB
020300060919     C                   exsr      wrivat                                       => scarico VAT
020400020305     C                   else                                                   => x stessa spediz
020500060919     C                   exsr      impvab                                       => carico VAB
020600060919     C                   exsr      wrivat                                       => scarico VAT
020700010305     C                   endif
020800010305     C                   endif
020900010305     C                   endif
021000000905     C*
021100000905     C                   else
021200000905     C                   eval      vinflg = '1'
021300000905     C                   endif
021400000905     C*
021500000905     C  N70              update    tivin000
021600000905     C*
021700991022     C  N70              ENDdo
021800010202     C*
021900020305     C* Scarico i VAB rimasti "in sospeso"
022000020305     C                   exsr      wrivab
022100010202     C*
022200010202     C                   endif
022300990910
022400990910     C* Se non ci sono record con errori ...
022500000710     C                   if        §ctrno = 0
022600990910     C* ... restituisco esito OK.
022700990921     C                   eval      wrkesito = '0'
022800990910     C                   else
022900010201     C                   if        §ctrokvb > 0
023000990921     C                   eval      wrkesito = '1'
023100000710     C                   else
023200000710     C                   eval      wrkesito = '2'
023300990910     C                   endif
023400000710     C                   endif
023500990910     C*
023600990914     C                   if        %open(tivin00r)
023700990908     C                   close     tivin00r
023800990914     C                   endif
023900021113     C                   if        %open(fivabwwr)
024000021113     C                   close     fivabwwr
024100990914     C                   endif
024200021113     C                   if        %open(fivatwwr)
024300021113     C                   close     fivatwwr
024400010201     C                   endif
024500990910     C*
024600010201     C                   if        §ctrokvb > 0
024700000724     C                             and vlrpoi <> *zeros
024800010202     C                   exsr      invio
024900990920     C                   endif
025000990920     C*
025100910830     C                   ENDSR
025200000613     C***
025300010305
025400010305     C*----------------------------------------------------*
025500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025600010305     C*----------------------------------------------------*
025700020305     C     WRIVAB        BEGSR
025800010305     C*
025900021113     C                   write     fivab000                                     => scarico il VAB
026000010305     C*
026100010305     C                   ENDSR
026200990920
026300000801     C*----------------------------------------------------*
026400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026500000801     C*----------------------------------------------------*
026600010201     C     INZVAR        BEGSR
026700000801     C*
026800010201     C                   Z-ADD     *zeros        Num5_0
026900000801     C*
027000000801     C                   ENDSR
027100000801     C*----------------------------------------------------*
027200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027300000801     C*----------------------------------------------------*
027400000801     C     DEFCAM        BEGSR
027500000801     C*
027600021113     C                   CLEAR                   FIVAB000
027700020619     C* Imposto i valori di default...
027800040714     C                   Z-ADD     0012223       VABCCM
027900040714     C                   Z-ADD     0012223       VATCCM
028000040714     C                   Z-ADD     001           VABLNP
028100040714     C                   Z-ADD     001           VATLNP
028200040714     C                   Z-ADD     000           VABCTR
028300060919     C                   MOVEL     '1'           VABCBO
028400040714     C                   MOVEL     '7Q'          VABCTM
028500020619     C* ... e poi verifico se sono stati passati come parametri
028600020619     C                   IF        vlrppt > *blanks
028700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028900020619     C                   EXSR      CHKNUM
029000020619     C                   IF        PiInt=*on
029100020619     C                   Z-ADD     PiVal         VABCCM
029200020619     C                   Z-ADD     PiVal         VATCCM
029300020619     C                   ENDIF
029400040506     C                   ENDIF
029500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABLNP
030000020619     C                   Z-ADD     PiVal         VATLNP
030100040506     C                   ENDIF
030200020619     C                   ENDIF
030300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030500020619     C                   EXSR      CHKNUM
030600020619     C                   IF        PiInt=*on
030700020619     C                   Z-ADD     PiVal         VABCTR
030800040506     C                   ENDIF
030900020619     C                   ENDIF
031000020619     C                   ENDIF
031100000801     C*
031200000801     C                   ENDSR
031300000801     C*----------------------------------------------------*
031400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031500000801     C*----------------------------------------------------*
031600010201     C     IMPVAB        BEGSR
031700010305     C*
031800000801     C                   Z-ADD     *zeros        errore            1 0
031900000830     C                   MOVEL     datcor        VABAAS
032000020305     C                   MOVEL     datcor        VATAAS
032100040526     C                   MOVE      datcor        VABMGS
032200021113     C                   MOVE(P)   vlrpoi        VABFGS
032300021113     C                   MOVE(P)   vlrpoi        VATFGS
032400040714     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
032500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032700020117     C* ==
032800040714     C                   EVAL      VABIND=%trim(%subst(vindta:57:50))
032900040526     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
033000040526     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
033100060919     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
033200040506     C* CAD
033300040526     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
033400010201     C                   EXSR      CHKNUM
033500010201     C                   IF        PiInt=*on
033600010201     C                   Z-ADD     PiVal         Num5_0
033700040506     C                   MOVEL(P)  Num5_0        VABCAD
033800010201     C                   ELSE
033900040506     C                   ADD       1             errore
034000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034100040506     C                             + ' ' + 'VABCAD'
034200010201     C                   ENDIF
034300040506     C* Reperisco la provincia dal CAP e dalla località
034400040526     C                   IF        VABCAD <> *blanks AND
034500040526     C                             VABPRD  = *blanks
034600040506     C                   CLEAR                   TISI95DS
034700040506     C                   EVAL      I95TCN = '3'
034800040506     C                   Z-ADD     datcor        I95DAT
034900040506     C                   EVAL      I95CAP = VABCAD
035000040506     C                   EVAL      I95LOC = VABLOD
035100040506     C                   CALL      'TISI95R'
035200040506     C                   PARM                    TISI95DS
035300040506     C                   EVAL      VABPRD = O95PRV
035400040506     C                   ENDIF
035500040506     C* NCL
035600040526     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
035700040506     C                   EXSR      CHKNUM
035800040506     C                   IF        PiInt=*on
035900060919     C                   ADD       PiVal         VABNCL
036000040506     C                   ELSE
036100040506     C                   ADD       1             errore
036200040506     C                   EVAL      vinmsg = %trimr(vinmsg)
036300040506     C                             + ' ' + 'VABNCL'
036400040506     C                   ENDIF
036500040506     C* PKB
036600060919     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
036700010201     C                   EXSR      CHKNUM
036800010201     C                   IF        PiNum=*on
036900060919     C                   ADD       PiVal         VABPKB
037000010201     C                   ELSE
037100010201     C                   ADD       1             errore
037200010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037300010201     C                             + ' ' + 'VABPKB'
037400010201     C                   ENDIF
037500040506     C* NSP/RMN
037600040526     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
037700010201     C                   EXSR      CHKNUM
037800010201     C                   IF        PiInt=*on
037900040506     C                   Z-ADD     PiVal         VABNSP
038000040506     C                   Z-ADD     PiVal         VATNSP
038100040506     C                   Z-ADD     PiVal         VABRMN
038200010201     C                   ELSE
038300010201     C                   ADD       1             errore
038400010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038600040526     C                   ENDIF
038700010205     C*
038800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038900010205     C                   IF        VABCAS > *zeros
039000010205     C                   EVAL      VABCBO = '4'
039100010205     C                   ELSE
039200010205     C                   EVAL      VABCBO = '1'
039300010205     C                   ENDIF
039400020305     C*
039500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039600011113     C                   EXSR      CHKIMPDIV
039700010202     C*
039800000801     C* Ebbene...
039900000801     C                   ADD       1             §CTRMO
040000010201     C                   IF        errore <> *zeros
040100000801     C                   ADD       1             §CTRNO
040200000801     C                   EVAL      recko = vindta
040300000801     C                   EXCEPT    rigadett
040400000801     C                   EVAL      vinflg = '2'
040500000801     C                   ELSE
040600010201     C                   ADD       1             §CTROKVB
040700000801     C                   ENDIF
040800000801     C*
040900000801     C                   ENDSR
041000060919     C*----------------------------------------------------*
041100060919     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041200060919     C*----------------------------------------------------*
041300060919     C     WRIVAT        BEGSR
041400060919     C*
041500060919     C* Valorizzo l buffer di scrittura del FIVAT
041600060919     C                   EVAL      VATTRC = 'E'
041700060919     C                   EVAL      VATNOT=%subst(vindta:331:14)
041800060919     C                   WRITE     FIVAT000
041900060919     C*
042000060919     C                   ENDSR
042100010202     C*----------------------------------------------------*
042200021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300010202     C*----------------------------------------------------*
042400020305     C     PREVAT        BEGSR
042500010202     C*
042600021113     C* Compongo il nome del membro da dare al FIVATWWR
042700010202     C                   eval      parmbr = vlrhdl
042800010202     C                   movel     'M'           parmbr
042900040714     C                   eval      parccm = '00012223'
043000010202     C                   eval      paropz = '1'
043100010202     C* Effettuo la chiamata al CLLE preposto
043200040506     C                   call(e)   'TITVVTC'
043300010202     C                   parm                    parccm
043400010202     C                   parm                    parmbr
043500010202     C                   parm                    paropz
043600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043700010202     C                   if        %error
043800010202     C                   movel     '1'           chkcall
043900010202     C                   else
044000010202     C                   movel     '0'           chkcall
044100010202     C                   endif
044200010202     C*
044300010202     C                   ENDSR
044400000801     C*----------------------------------------------------*
044500000801     C*  CONTROLLO NUMERICITA' CAMPI
044600000801     C*----------------------------------------------------*
044700000801     C     CHKNUM        BEGSR
044800000801     C*
044900000801     C                   call(e)   'ISNUMERIC'
045000000801     C                   PARM                    PiStr            30
045100040714     C                   PARM      ','           PiDecChr          1
045200000801     C                   PARM      *ZEROS        PiVal            30 9
045300000801     C                   PARM      '0'           PiInt             1
045400000801     C                   PARM      '0'           PiNum             1
045500000801     C                   IF        %error
045600000801     C                   EVAL      PiInt=*off
045700000801     C                   ENDIF
045800000801     C*
045900000801     C                   ENDSR
046000000801     C***
046100000801
046200011113
046300011113     C*----------------------------------------------------*
046400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046500011113     C*----------------------------------------------------*
046600011113     C     CHKIMPDIV     BEGSR
046700011113     C*
046800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046900011113     C                   Z-ADD     *zeros        wrkDec            9 9
047000011113     C*
047100011113     C* Come prima cosa effettuo considerazioni sulla divisa
047200011113     C                   IF        vabIAS > *zeros
047300011113     C                   IF        vabVAS <> 'EUR'
047400011113     C                   EVAL      vabVAS =  'ITL'
047500011113     C                   ENDIF
047600011113     C                   ENDIF
047700011113     C*
047800011113     C                   IF        vabCAS > *zeros
047900011113     C                   IF        vabVCA <> 'EUR'
048000011113     C                   EVAL      vabVCA =  'ITL'
048100011113     C                   ENDIF
048200011113     C                   ENDIF
048300011113     C*
048400011113     C                   IF        vabVMD > *zeros
048500020305     C                   IF        vabVAD <> 'EUR'
048600011113     C                   EVAL      vabVAD =  'ITL'
048700011113     C                   ENDIF
048800011113     C                   ENDIF
048900011113     C*
049000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049100011113     C                   Z-ADD     vabIAS        wrkDec
049200011113     C                   IF        wrkDec > *zeros
049300011113     C                   IF        vabVAS = 'ITL'
049400011113     C                   EVAL      vabIAS = *zeros
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049900011113     C                   Z-ADD     vabCAS        wrkDec
050000011113     C                   IF        wrkDec > *zeros
050100011113     C                   IF        vabVCA = 'ITL'
050200011113     C                   EVAL      vabCAS = *zeros
050300011113     C                   ENDIF
050400011113     C                   ENDIF
050500011113     C*
050600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050700011113     C                   Z-ADD     vabVMD        wrkDec
050800011113     C                   IF        wrkDec > *zeros
050900011113     C                   IF        vabVAD = 'ITL'
051000011113     C                   EVAL      vabVMD = *zeros
051100011113     C                   ENDIF
051200011113     C                   ENDIF
051300011113     C*
051400011113     C                   ENDSR
051500011113     C***
051600011113
051700011113
051800000801
051900000801
052000990920      /TITLE Invio dei dati al punto operativo.
052100010202     C     invio         BEGSR
052200990920     C*
052300021113     C* 1° invio FIVAT
052400010201     C                   reset                   dscmz
052500010201     C                   move      vlrpoi        cmzdst
052600021113     C                   eval      cmzfld = 'FIVATWWR'
052700010201     C                   eval      cmzmbd = vlrhdl
052800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052900021009     C***                if        prmfir = *blanks
053000021113     C                   eval      cmzfla = 'FIVAT00F'
053100021113     C                   eval      cmzmba = 'FIVAT00F'
053200021009     C***                else
053300021009     C***                eval      cmzfla = prmfir
053400021009     C***                eval      cmzmba = prmfir
053500021009     C***                endif
053600010201     C                   eval      cmznrr = *zeros
053700020305     C                   move      §ctrokvt      cmznrr
053800021018     C                   eval      cmzlba = vlrfl1
053900010201     C                   call(e)   'TIS711C'
054000010201     C                   parm                    dscmz
054100010201     C                   parm      *blanks       esito
054200010205     C                   if        %error
054300010205     C                             or cmzerr = '1'
054400010205     C                             or esito  = '1'
054500010205     C                   eval      wrkesito = '3'
054600010205     C                   else
054700010201     C*
054800021113     C* 2° invio FIVAB
054900010201     C                   reset                   dscmz
055000010201     C                   move      vlrpoi        cmzdst
055100010201     C                   eval      cmzfld = vlrfou
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500021113     C                   eval      cmzfla = 'FIVAB00F'
055600021113     C                   eval      cmzmba = 'FIVAB00F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200010201     C                   move      §ctrokvb      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010201     C                   if        %error
056800010201     C                             or cmzerr = '1'
056900010201     C                             or esito  = '1'
057000010201     C                   eval      wrkesito = '3'
057100010201     C                   endif
057200010205     C                   endif
057300990920     C*
057400000613     C                   ENDSR
057500000613     C***
057600070411
057700070411     C     *pssr         BEGSR
057800070411     C*
057900070411     C                   if        %open(tivin00r)
058000070411     C                   close     tivin00r
058100070411     C                   endif
058200070411     C                   if        %open(fivabwwr)
058300070411     C                   close     fivabwwr
058400070411     C                   endif
058500070411     C                   if        %open(fivatwwr)
058600070411     C                   close     fivatwwr
058700070411     C                   endif
058800070411     C*
058900070411     C* Effettuo la chiamata al CLLE preposto
059000070411     C                   call(e)   'TITVVTC'
059100070411     C                   parm                    parccm
059200070411     C                   parm                    parmbr
059300070411     C                   parm      '2'           paropz
059400070411     C*
059500070411     C                   eval      wrkesito = '2'
059600070411     C*
059700070411     C                   seton                                        LR
059800070411     C*
059900070411     C                   ENDSR     '*CANCL'
060000070411     C***
060100990910
060200000613     C     *inzsr        BEGSR
060300990910     C*
060400990910     C     *entry        plist
060500990920     C                   parm                    tivlrds
060600990921     C                   parm      wrkesito      esito
060700000724     C                   parm                    prmlit
060800000710     C                   parm                    prmfir
060900000613     C*
061000000830     C* CALCOLA LA DATA CORRENTE
061100000830     C                   time                    wn14             14 0
061200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061300000830     C                   z-add     wn8           g08dat
061400000830     C                   z-add     *zeros        g08inv
061500000830     C                   movel     '0'           g08err
061600000830     C                   call      'XSRDA8'
061700000830     C                   parm                    wlbda8
061800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061900000830     C*
062000000613     C                   ENDSR
062100000613     C***
062200990908
062300040714     Otitvd6p   E            riepilogo         2
062400990915     O                                              'Upload via Internet'
062500990915     O                                           +1 'Traduzione TIVIN00R -'
062600021113     O                                           +1 'FIVABWWR/FIVATWWR'
062700010201     O                                           +1 'Report testate bolle'
062800990915     O          E            riepilogo   2
062900990915     O                       wrkdata
063000990915     O                       wrkora              +1
063100990915     O                       procname            +1
063200990915     O          E            riepilogo   2
063300990915     O                                              'Cliente..................:'
063400990915     O                       VABCCM        z     +1
063500990915     O          E            riepilogo   2
063600990920     O                                              'Riferimento Strategi.....:'
063700990920     O                       vlrhdl              +1
063800990915     O          E            riepilogo   2
063900990915     O                                              'Giusti...................:'
064000010201     O                       §CTROKVB      2   +  1
064100990915     O          E            riepilogo   2
064200990915     O                                              'Sbagliati e corretti.....:'
064300971022     O                       §CTRMO        2   +  1
064400990915     O          E            riepilogo   2
064500990915     O                                              'Sbagliati e scartati.....:'
064600971022     O                       §CTRNO        2   +  1
064700000613
064800040714     Otitvd6ps  E            testdett          2
064900000613     O                                              'Upload via Internet'
065000000613     O                                           +1 'Traduzione TIVIN00R -'
065100021113     O                                           +1 'FIVABWWR/FIVATWWR'
065200010201     O                                           +1 'Report testate bolle'
065300000616     O          E            testdett    3
065400000613     O                                           +2 'N° rec'
065500000613     O                                           +3 'Anteprima contenuto'
065600000616     O          E            rigadett    2
065700000613     O                       rrnum               +2
065800000621     O                       recko               +3
065900000616     O          E            findett     2
066000000613     O                       wrkdata
066100000613     O                       wrkora              +1
066200000613     O                       procname            +1
066300000616     O          E            findett     2
066400000613     O                                              'Cliente..................:'
066500000613     O                       VABCCM        z     +1
066600000616     O          E            findett     2
066700000613     O                                              'Riferimento Strategi.....:'
066800000613     O                       vlrhdl              +1
066900000616     O          E            findett     2
067000000613     O                                              'Giusti...................:'
067100010201     O                       §CTROKVB      2   +  1
067200000616     O          E            findett     2
067300000613     O                                              'Sbagliati e corretti.....:'
067400000613     O                       §CTRMO        2   +  1
067500000616     O          E            findett     2
067600000613     O                                              'Sbagliati e scartati.....:'
067700000613     O                       §CTRNO        2   +  1
067800000512** CMD - COMANDI CL
067900040714OVRPRTF FILE(TITVD6P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068000040714OVRPRTF FILE(TITVD6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068100040714DLTOVR FILE(TITVD6P TITVD6PS) LVL(*)
068200000512
068300000512
