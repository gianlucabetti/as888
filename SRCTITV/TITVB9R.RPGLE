000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040526     Ftitvb9p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040526     Ftitvb9ps  O    f  198        PRINTER usropn
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
003700010213     D depdat          s              8  0 INZ(*zeros)
003800040526     D curspe          s             10    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300010213
004400000801     D  Num5_0         s              5  0
004500050228     D  wPrgColloNum   s              5  0
004600050228     D  wPrgColloAlf   s              5
004700000830
004800000830     D*------------------
004900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000000830     D*------------------
005100000830     D WLBDA8          DS                  INZ
005200000830     D  G08DAT                 1      8  0
005300000830     D  G08INV                 9     16  0
005400000830     D  G08ERR                17     17
005500000830     D  G08TGI                18     22  0
005600000830     D*
005700990908
005800010201
005900010201
006000990915     C                   time                    wrkdata
006100990915     C                   time                    wrkora
006200000913     C                   reset                   rrnum
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500000724     C*
006600000724     C* SE OCCORRE SPEDIRE IN FILIALE
006700000724     C                   if        vlrpoi <> *zeros
006800000724     C*
006900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007100000724     C     vlrpoi        chain     azorg01l
007200000724     C                   if        %found
007300000616     C                   movel(p)  CMD(1)        depcmd
007400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007500000616     C*
007600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007700011113     C                   Z-ADD     150           LENGH            15 5
007800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007900000616     C                   PARM                    depcmd
008000000616     C                   PARM                    LENGH
008100000724     C*
008200000724     C                   endif
008300000724     C                   endif
008400000616     C*
008500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008600000616     C                   movel(p)  CMD(2)        depcmd
008700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008800000616     C*
008900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009000011113     C                   Z-ADD     150           LENGH            15 5
009100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009200000616     C                   PARM                    depcmd
009300000616     C                   PARM                    LENGH
009400000616     C*
009500040526     C                   if        not %open(titvb9ps)
009600040526     C                   open      titvb9ps
009700000616     C                   except    testdett
009800000613     C                   endif
009900000613     C*
010000040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010200000613     C*
010300010202     C* Effettuo la chiamata al CLLE preposto
010400040506     C                   call(e)   'TITVVTC'
010500010202     C                   parm                    parccm
010600010202     C                   parm                    parmbr
010700010202     C                   parm      '2'           paropz
010800050201     C*
010900050201     C* Effettuo lancio TISI95 solo x chiusura
011000050201     C                   CLEAR                   TISI95DS
011100050201     C                   EVAL      I95TLA = 'C'
011200050201     C                   CALL      'TISI95R'
011300050201     C                   PARM                    TISI95DS
011400010202     C*
011500040526     C                   if        %open(titvb9ps)
011600000616     C                   except    findett
011700040526     C                   close     titvb9ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600000801     C
012700010201     C                   seton                                        LR
012800000613
012900000613
013000000613     C*--------------------------------------------------------
013100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013200000613     C*--------------------------------------------------------
013300000621     C     STPR          BEGSR
013400000613     C*
013500040526     C                   if        not %open(titvb9p)
013600040526     C                   open      titvb9p
013700990915     C                   endif
013800990915     C*
013900990915     C                   except    riepilogo
014000990915     C*
014100040526     C                   if        %open(titvb9p)
014200040526     C                   close     titvb9p
014300990914     C                   endif
014400990910     C*
014500000613     C                   ENDSR
014600000613     C***
014700990908
014800000801
014900910830     C*--------------------------------------------------------
015000040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
015100910830     C*--------------------------------------------------------
015200040526     C     RWFILE        BEGSR
015300990910     C*
015400990914     C                   if        not %open(tivin00r)
015500990908     C                   open      tivin00r
015600990914     C                   endif
015700021113     C                   if        not %open(fivabwwr)
015800021113     C                   open      fivabwwr
015900990914     C                   endif
016000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100020305     C                   exsr      prevat
016200010201     C*
016300010202     C                   if        chkcall = '0'
016400010202     C*
016500021113     C                   if        not %open(fivatwwr)
016600021113     C                   open      fivatwwr
016700010201     C                   endif
016800990910     C*
016900010201     C                   clear                   §CTROKVB          5 0
017000020305     C                   clear                   §CTROKVT          5 0
017100000801     C                   clear                   §CTRMO            5 0
017200000801     C                   clear                   §CTRNO            5 0
017300990910     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700000905     C                   if        vindta > *blanks
017800000613     C                   add       1             rrnum
017900000801     C*
018000000801     C                   if        *in70 = *off
018100000801     C                             and
018200000801     C                             (vinflg = *blanks
018300000801     C                              or vinflg = '0'
018400000801     C                              or vinflg = '2')
018500000801     C*
018600000801     C                   clear                   vinmsg
018700000801     C                   eval      vinflg = '1'
018800010305     C*
018900010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
019000040526     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
019100020305     C                   MOVEL(p)  PiStr         curspe
019200010305     C*
019300010305     C                   if        depspe = *blanks                             => 1° giro
019400010305     C                   eval      depspe = curspe                              => memorizz. spediz
019500020305     C                   exsr      impvab
019600020305     C                   exsr      wrivat                                       => carico VAT
019700010305     C                   else
019800010305     C                   if        curspe <> depspe                             => rottura di spediz
019900010305     C                   eval      depspe = curspe                              => memorizz. spediz
020000020305     C                   exsr      wrivab
020100020305     C                   exsr      impvab
020200020305     C                   exsr      wrivat                                       => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400050228     C                   exsr      wrivat                                       => carico VAT
020500010305     C                   endif
020600010305     C                   endif
020700010305     C                   endif
020800000905     C*
020900000905     C                   else
021000000905     C                   eval      vinflg = '1'
021100000905     C                   endif
021200000905     C*
021300000905     C  N70              update    tivin000
021400000905     C*
021500991022     C  N70              ENDdo
021600010202     C*
021700020305     C* Scarico i VAB rimasti "in sospeso"
021800020305     C                   exsr      wrivab
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021113     C                   if        %open(fivabwwr)
023800021113     C                   close     fivabwwr
023900990914     C                   endif
024000021113     C                   if        %open(fivatwwr)
024100021113     C                   close     fivatwwr
024200010201     C                   endif
024300990910     C*
024400010201     C                   if        §ctrokvb > 0
024500000724     C                             and vlrpoi <> *zeros
024600010202     C                   exsr      invio
024700990920     C                   endif
024800990920     C*
024900910830     C                   ENDSR
025000000613     C***
025100010305
025200010305     C*----------------------------------------------------*
025300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025400010305     C*----------------------------------------------------*
025500020305     C     WRIVAB        BEGSR
025600010305     C*
025700021113     C                   write     fivab000                                     => scarico il VAB
025800010305     C*
025900010305     C                   ENDSR
026000990920
026100000801     C*----------------------------------------------------*
026200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300000801     C*----------------------------------------------------*
026400010201     C     INZVAR        BEGSR
026500000801     C*
026600010201     C                   Z-ADD     *zeros        Num5_0
026700050228     C                   Z-ADD     *zeros        wPrgColloNum
026800050228     C                   MOVEL     *blanks       wPrgColloAlf
026900000801     C*
027000000801     C                   ENDSR
027100000801     C*----------------------------------------------------*
027200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027300000801     C*----------------------------------------------------*
027400000801     C     DEFCAM        BEGSR
027500000801     C*
027600021113     C                   CLEAR                   FIVAB000
027700050309     C                   CLEAR                   FIVAT000
027800020619     C* Imposto i valori di default...
027900040526     C                   Z-ADD     0432607       VABCCM
028000040526     C                   Z-ADD     0432607       VATCCM
028100050316     C                   Z-ADD     043           VABLNP
028200050316     C                   Z-ADD     043           VATLNP
028300040527     C                   Z-ADD     301           VABCTR
028400050228     C                   MOVEL     '7Q'          VABCTM
028500050228     C                   MOVEL     'E'           VATTRC
028600020619     C* ... e poi verifico se sono stati passati come parametri
028700020619     C                   IF        vlrppt > *blanks
028800040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
028900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029000020619     C                   EXSR      CHKNUM
029100020619     C                   IF        PiInt=*on
029200020619     C                   Z-ADD     PiVal         VABCCM
029300020619     C                   Z-ADD     PiVal         VATCCM
029400020619     C                   ENDIF
029500040506     C                   ENDIF
029600040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
029700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029800020619     C                   EXSR      CHKNUM
029900020619     C                   IF        PiInt=*on
030000020619     C                   Z-ADD     PiVal         VABLNP
030100020619     C                   Z-ADD     PiVal         VATLNP
030200040506     C                   ENDIF
030300020619     C                   ENDIF
030400040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
030500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030600020619     C                   EXSR      CHKNUM
030700020619     C                   IF        PiInt=*on
030800020619     C                   Z-ADD     PiVal         VABCTR
030900040506     C                   ENDIF
031000020619     C                   ENDIF
031100020619     C                   ENDIF
031200000801     C*
031300000801     C                   ENDSR
031400000801     C*----------------------------------------------------*
031500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031600000801     C*----------------------------------------------------*
031700010201     C     IMPVAB        BEGSR
031800000801     C*
031900020305     C                   EXSR      INZVAR
032000020305     C                   EXSR      DEFCAM
032100010305     C*
032200000801     C                   Z-ADD     *zeros        errore            1 0
032300000830     C                   MOVEL     datcor        VABAAS
032400020305     C                   MOVEL     datcor        VATAAS
032500040526     C                   MOVE      datcor        VABMGS
032600021113     C                   MOVE(P)   vlrpoi        VABFGS
032700021113     C                   MOVE(P)   vlrpoi        VATFGS
032800040526     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
032900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
033000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
033100020117     C* ==
033200040526     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
033300040526     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
033400040526     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
033500040526     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
033600040526     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
033700040526     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
033800040526     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:35))
033900040506     C* CAD
034000040526     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
034100010201     C                   EXSR      CHKNUM
034200010201     C                   IF        PiInt=*on
034300010201     C                   Z-ADD     PiVal         Num5_0
034400040506     C                   MOVEL(P)  Num5_0        VABCAD
034500010201     C                   ELSE
034600040506     C                   ADD       1             errore
034700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034800040506     C                             + ' ' + 'VABCAD'
034900010201     C                   ENDIF
035000040506     C* Reperisco la provincia dal CAP e dalla località
035100040526     C                   IF        VABCAD <> *blanks AND
035200040526     C                             VABPRD  = *blanks
035300040506     C                   CLEAR                   TISI95DS
035400040506     C                   EVAL      I95TCN = '3'
035500040506     C                   Z-ADD     datcor        I95DAT
035600040506     C                   EVAL      I95CAP = VABCAD
035700040506     C                   EVAL      I95LOC = VABLOD
035800040506     C                   CALL      'TISI95R'
035900040506     C                   PARM                    TISI95DS
036000040506     C                   EVAL      VABPRD = O95PRV
036100040506     C                   ENDIF
036200040506     C* NCL
036300040526     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
036400040506     C                   EXSR      CHKNUM
036500040506     C                   IF        PiInt=*on
036600040506     C                   Z-ADD     PiVal         VABNCL
036700040506     C                   ELSE
036800040506     C                   ADD       1             errore
036900040506     C                   EVAL      vinmsg = %trimr(vinmsg)
037000040506     C                             + ' ' + 'VABNCL'
037100040506     C                   ENDIF
037200040506     C* PKB
037300040526     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
037400010201     C                   EXSR      CHKNUM
037500010201     C                   IF        PiNum=*on
037600011115     C                   Z-ADD     PiVal         VABPKB
037700010201     C                   ELSE
037800010201     C                   ADD       1             errore
037900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038000010201     C                             + ' ' + 'VABPKB'
038100010201     C                   ENDIF
038200040506     C* NSP/RMN
038300040526     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
038400010201     C                   EXSR      CHKNUM
038500010201     C                   IF        PiInt=*on
038600040506     C                   Z-ADD     PiVal         VABNSP
038700040506     C                   Z-ADD     PiVal         VATNSP
038800040506     C                   Z-ADD     PiVal         VABRMN
038900010201     C                   ELSE
039000010201     C                   ADD       1             errore
039100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
039200040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
039300040526     C                   ENDIF
039400010205     C*
039500010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
039600010205     C                   IF        VABCAS > *zeros
039700010205     C                   EVAL      VABCBO = '4'
039800010205     C                   ELSE
039900010205     C                   EVAL      VABCBO = '1'
040000010205     C                   ENDIF
040100020305     C*
040200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040300011113     C                   EXSR      CHKIMPDIV
040400010202     C*
040500000801     C* Ebbene...
040600000801     C                   ADD       1             §CTRMO
040700010201     C                   IF        errore <> *zeros
040800000801     C                   ADD       1             §CTRNO
040900000801     C                   EVAL      recko = vindta
041000000801     C                   EXCEPT    rigadett
041100000801     C                   EVAL      vinflg = '2'
041200000801     C                   ELSE
041300010201     C                   ADD       1             §CTROKVB
041400000801     C                   ENDIF
041500000801     C*
041600000801     C                   ENDSR
041700010201     C*----------------------------------------------------*
041800021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041900010201     C*----------------------------------------------------*
042000020305     C     WRIVAT        BEGSR
042100010201     C*
042200021113     C* Valorizzo l buffer di scrittura del FIVAT
042300050228     C                   EVAL      VATTRC = 'E'
042400050228     C                   ADD       1             wPrgColloNum
042500050228     C                   MOVE(P)   wPrgColloNum  wPrgColloAlf
042600050228     C                   EVAL      VATNOT = %subst(vindta:187:10) + wPrgColloAlf
042700050228     C                   WRITE     FIVAT000
042800040526     C                   IF        %subst(vindta:92:15) <> *blanks
042900040506     C                   EVAL      VATTRC = 'B'
043000040526     C                   EVAL      VATNOT=%subst(vindta:92:15)
043100040526     C                   WRITE     FIVAT000
043200040506     C                   ENDIF
043300010201     C*
043400010201     C                   ENDSR
043500010202     C*----------------------------------------------------*
043600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043700010202     C*----------------------------------------------------*
043800020305     C     PREVAT        BEGSR
043900010202     C*
044000021113     C* Compongo il nome del membro da dare al FIVATWWR
044100010202     C                   eval      parmbr = vlrhdl
044200010202     C                   movel     'M'           parmbr
044300040526     C                   eval      parccm = '00432607'
044400010202     C                   eval      paropz = '1'
044500010202     C* Effettuo la chiamata al CLLE preposto
044600040506     C                   call(e)   'TITVVTC'
044700010202     C                   parm                    parccm
044800010202     C                   parm                    parmbr
044900010202     C                   parm                    paropz
045000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045100010202     C                   if        %error
045200010202     C                   movel     '1'           chkcall
045300010202     C                   else
045400010202     C                   movel     '0'           chkcall
045500010202     C                   endif
045600010202     C*
045700010202     C                   ENDSR
045800000801     C*----------------------------------------------------*
045900000801     C*  CONTROLLO NUMERICITA' CAMPI
046000000801     C*----------------------------------------------------*
046100000801     C     CHKNUM        BEGSR
046200000801     C*
046300000801     C                   call(e)   'ISNUMERIC'
046400000801     C                   PARM                    PiStr            30
046500020619     C                   PARM      '.'           PiDecChr          1
046600000801     C                   PARM      *ZEROS        PiVal            30 9
046700000801     C                   PARM      '0'           PiInt             1
046800000801     C                   PARM      '0'           PiNum             1
046900000801     C                   IF        %error
047000000801     C                   EVAL      PiInt=*off
047100000801     C                   ENDIF
047200000801     C*
047300000801     C                   ENDSR
047400000801     C***
047500000801
047600011113
047700011113     C*----------------------------------------------------*
047800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047900011113     C*----------------------------------------------------*
048000011113     C     CHKIMPDIV     BEGSR
048100011113     C*
048200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048300011113     C                   Z-ADD     *zeros        wrkDec            9 9
048400011113     C*
048500011113     C* Come prima cosa effettuo considerazioni sulla divisa
048600011113     C                   IF        vabIAS > *zeros
048700011113     C                   IF        vabVAS <> 'EUR'
048800011113     C                   EVAL      vabVAS =  'ITL'
048900011113     C                   ENDIF
049000011113     C                   ENDIF
049100011113     C*
049200011113     C                   IF        vabCAS > *zeros
049300011113     C                   IF        vabVCA <> 'EUR'
049400011113     C                   EVAL      vabVCA =  'ITL'
049500011113     C                   ENDIF
049600011113     C                   ENDIF
049700011113     C*
049800011113     C                   IF        vabVMD > *zeros
049900020305     C                   IF        vabVAD <> 'EUR'
050000011113     C                   EVAL      vabVAD =  'ITL'
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050500011113     C                   Z-ADD     vabIAS        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVAS = 'ITL'
050800011113     C                   EVAL      vabIAS = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051300011113     C                   Z-ADD     vabCAS        wrkDec
051400011113     C                   IF        wrkDec > *zeros
051500011113     C                   IF        vabVCA = 'ITL'
051600011113     C                   EVAL      vabCAS = *zeros
051700011113     C                   ENDIF
051800011113     C                   ENDIF
051900011113     C*
052000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052100011113     C                   Z-ADD     vabVMD        wrkDec
052200011113     C                   IF        wrkDec > *zeros
052300011113     C                   IF        vabVAD = 'ITL'
052400011113     C                   EVAL      vabVMD = *zeros
052500011113     C                   ENDIF
052600011113     C                   ENDIF
052700011113     C*
052800011113     C                   ENDSR
052900011113     C***
053000011113
053100011113
053200000801
053300000801
053400990920      /TITLE Invio dei dati al punto operativo.
053500010202     C     invio         BEGSR
053600990920     C*
053700021113     C* 1° invio FIVAT
053800010201     C                   reset                   dscmz
053900010201     C                   move      vlrpoi        cmzdst
054000021113     C                   eval      cmzfld = 'FIVATWWR'
054100010201     C                   eval      cmzmbd = vlrhdl
054200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054300021009     C***                if        prmfir = *blanks
054400021113     C                   eval      cmzfla = 'FIVAT00F'
054500021113     C                   eval      cmzmba = 'FIVAT00F'
054600021009     C***                else
054700021009     C***                eval      cmzfla = prmfir
054800021009     C***                eval      cmzmba = prmfir
054900021009     C***                endif
055000010201     C                   eval      cmznrr = *zeros
055100020305     C                   move      §ctrokvt      cmznrr
055200021018     C                   eval      cmzlba = vlrfl1
055300010201     C                   call(e)   'TIS711C'
055400010201     C                   parm                    dscmz
055500010201     C                   parm      *blanks       esito
055600010205     C                   if        %error
055700010205     C                             or cmzerr = '1'
055800010205     C                             or esito  = '1'
055900010205     C                   eval      wrkesito = '3'
056000010205     C                   else
056100010201     C*
056200021113     C* 2° invio FIVAB
056300010201     C                   reset                   dscmz
056400010201     C                   move      vlrpoi        cmzdst
056500010201     C                   eval      cmzfld = vlrfou
056600010201     C                   eval      cmzmbd = vlrhdl
056700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056800021009     C***                if        prmfir = *blanks
056900021113     C                   eval      cmzfla = 'FIVAB00F'
057000021113     C                   eval      cmzmba = 'FIVAB00F'
057100021009     C***                else
057200021009     C***                eval      cmzfla = prmfir
057300021009     C***                eval      cmzmba = prmfir
057400021009     C***                endif
057500010201     C                   eval      cmznrr = *zeros
057600010201     C                   move      §ctrokvb      cmznrr
057700021018     C                   eval      cmzlba = vlrfl1
057800010201     C                   call(e)   'TIS711C'
057900010201     C                   parm                    dscmz
058000010201     C                   parm      *blanks       esito
058100010201     C                   if        %error
058200010201     C                             or cmzerr = '1'
058300010201     C                             or esito  = '1'
058400010201     C                   eval      wrkesito = '3'
058500010201     C                   endif
058600010205     C                   endif
058700990920     C*
058800000613     C                   ENDSR
058900000613     C***
059000070411
059100070411     C     *pssr         BEGSR
059200070411     C*
059300070411     C                   if        %open(tivin00r)
059400070411     C                   close     tivin00r
059500070411     C                   endif
059600070411     C                   if        %open(fivabwwr)
059700070411     C                   close     fivabwwr
059800070411     C                   endif
059900070411     C                   if        %open(fivatwwr)
060000070411     C                   close     fivatwwr
060100070411     C                   endif
060200070411     C*
060300070411     C* Effettuo la chiamata al CLLE preposto
060400070411     C                   call(e)   'TITVVTC'
060500070411     C                   parm                    parccm
060600070411     C                   parm                    parmbr
060700070411     C                   parm      '2'           paropz
060800070411     C*
060900070411     C                   eval      wrkesito = '2'
061000070411     C*
061100070411     C                   seton                                        LR
061200070411     C*
061300070411     C                   ENDSR     '*CANCL'
061400070411     C***
061500070411
061600990910
061700000613     C     *inzsr        BEGSR
061800990910     C*
061900990910     C     *entry        plist
062000990920     C                   parm                    tivlrds
062100990921     C                   parm      wrkesito      esito
062200000724     C                   parm                    prmlit
062300000710     C                   parm                    prmfir
062400000613     C*
062500000830     C* CALCOLA LA DATA CORRENTE
062600000830     C                   time                    wn14             14 0
062700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062800000830     C                   z-add     wn8           g08dat
062900000830     C                   z-add     *zeros        g08inv
063000000830     C                   movel     '0'           g08err
063100000830     C                   call      'XSRDA8'
063200000830     C                   parm                    wlbda8
063300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063400000830     C*
063500000613     C                   ENDSR
063600000613     C***
063700990908
063800040526     Otitvb9p   E            riepilogo         2
063900990915     O                                              'Upload via Internet'
064000990915     O                                           +1 'Traduzione TIVIN00R -'
064100021113     O                                           +1 'FIVABWWR/FIVATWWR'
064200010201     O                                           +1 'Report testate bolle'
064300990915     O          E            riepilogo   2
064400990915     O                       wrkdata
064500990915     O                       wrkora              +1
064600990915     O                       procname            +1
064700990915     O          E            riepilogo   2
064800990915     O                                              'Cliente..................:'
064900990915     O                       VABCCM        z     +1
065000990915     O          E            riepilogo   2
065100990920     O                                              'Riferimento Strategi.....:'
065200990920     O                       vlrhdl              +1
065300990915     O          E            riepilogo   2
065400990915     O                                              'Giusti...................:'
065500010201     O                       §CTROKVB      2   +  1
065600990915     O          E            riepilogo   2
065700990915     O                                              'Sbagliati e corretti.....:'
065800971022     O                       §CTRMO        2   +  1
065900990915     O          E            riepilogo   2
066000990915     O                                              'Sbagliati e scartati.....:'
066100971022     O                       §CTRNO        2   +  1
066200000613
066300040526     Otitvb9ps  E            testdett          2
066400000613     O                                              'Upload via Internet'
066500000613     O                                           +1 'Traduzione TIVIN00R -'
066600021113     O                                           +1 'FIVABWWR/FIVATWWR'
066700010201     O                                           +1 'Report testate bolle'
066800000616     O          E            testdett    3
066900000613     O                                           +2 'N° rec'
067000000613     O                                           +3 'Anteprima contenuto'
067100000616     O          E            rigadett    2
067200000613     O                       rrnum               +2
067300000621     O                       recko               +3
067400000616     O          E            findett     2
067500000613     O                       wrkdata
067600000613     O                       wrkora              +1
067700000613     O                       procname            +1
067800000616     O          E            findett     2
067900000613     O                                              'Cliente..................:'
068000000613     O                       VABCCM        z     +1
068100000616     O          E            findett     2
068200000613     O                                              'Riferimento Strategi.....:'
068300000613     O                       vlrhdl              +1
068400000616     O          E            findett     2
068500000613     O                                              'Giusti...................:'
068600010201     O                       §CTROKVB      2   +  1
068700000616     O          E            findett     2
068800000613     O                                              'Sbagliati e corretti.....:'
068900000613     O                       §CTRMO        2   +  1
069000000616     O          E            findett     2
069100000613     O                                              'Sbagliati e scartati.....:'
069200000613     O                       §CTRNO        2   +  1
069300000512** CMD - COMANDI CL
069400040526OVRPRTF FILE(TITVB9P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069500040526OVRPRTF FILE(TITVB9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
069600040526DLTOVR FILE(TITVB9P TITVB9PS) LVL(*)
069700000512
069800000512
