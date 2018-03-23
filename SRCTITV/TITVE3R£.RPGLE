000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800040802     Ftitve3p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100040802     Ftitve3ps  O    f  198        PRINTER usropn
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
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900000830     D*
005000990908
005100010201
005200010201
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000000724     C                   if        vlrpoi <> *zeros
006100000724     C*
006200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000724     C     vlrpoi        chain     azorg01l
006500000724     C                   if        %found
006600000616     C                   movel(p)  CMD(1)        depcmd
006700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006800000616     C*
006900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007000011113     C                   Z-ADD     150           LENGH            15 5
007100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200000616     C                   PARM                    depcmd
007300000616     C                   PARM                    LENGH
007400000724     C*
007500000724     C                   endif
007600000724     C                   endif
007700000616     C*
007800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000616     C                   movel(p)  CMD(2)        depcmd
008000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000616     C*
008800040802     C                   if        not %open(titve3ps)
008900040802     C                   open      titve3ps
009000000616     C                   except    testdett
009100000613     C                   endif
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009500000613     C*
009600010202     C* Effettuo la chiamata al CLLE preposto
009700040506     C                   call(e)   'TITVVTC'
009800010202     C                   parm                    parccm
009900010202     C                   parm                    parmbr
010000010202     C                   parm      '2'           paropz
010100050201     C*
010200050201     C* Effettuo lancio TISI95 solo x chiusura
010300050201     C                   CLEAR                   TISI95DS
010400050201     C                   EVAL      I95TLA = 'C'
010500050201     C                   CALL      'TISI95R'
010600050201     C                   PARM                    TISI95DS
010700010202     C*
010800040802     C                   if        %open(titve3ps)
010900000616     C                   except    findett
011000040802     C                   close     titve3ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900000801     C
012000010201     C                   seton                                        LR
012100000613
012200000613
012300000613     C*--------------------------------------------------------
012400000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012500000613     C*--------------------------------------------------------
012600000621     C     STPR          BEGSR
012700000613     C*
012800040802     C                   if        not %open(titve3p)
012900040802     C                   open      titve3p
013000990915     C                   endif
013100990915     C*
013200990915     C                   except    riepilogo
013300990915     C*
013400040802     C                   if        %open(titve3p)
013500040802     C                   close     titve3p
013600990914     C                   endif
013700990910     C*
013800000613     C                   ENDSR
013900000613     C***
014000990908
014100000801
014200910830     C*--------------------------------------------------------
014300040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014400910830     C*--------------------------------------------------------
014500040526     C     RWFILE        BEGSR
014600990910     C*
014700990914     C                   if        not %open(tivin00r)
014800990908     C                   open      tivin00r
014900990914     C                   endif
015000021113     C                   if        not %open(fivabwwr)
015100021113     C                   open      fivabwwr
015200990914     C                   endif
015300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015400020305     C                   exsr      prevat
015500010201     C*
015600010202     C                   if        chkcall = '0'
015700010202     C*
015800021113     C                   if        not %open(fivatwwr)
015900021113     C                   open      fivatwwr
016000010201     C                   endif
016100990910     C*
016200010201     C                   clear                   §CTROKVB          5 0
016300020305     C                   clear                   §CTROKVT          5 0
016400000801     C                   clear                   §CTRMO            5 0
016500000801     C                   clear                   §CTRNO            5 0
016600990910     C*
016700921023     C                   DO        *HIVAL
016800990913     C*
016900990915     C                   READ      tivin00r                               70
017000000905     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300000801     C                   if        *in70 = *off
017400000801     C                             and
017500000801     C                             (vinflg = *blanks
017600000801     C                              or vinflg = '0'
017700000801     C                              or vinflg = '2')
017800000801     C*
017900000801     C                   clear                   vinmsg
018000000801     C                   eval      vinflg = '1'
018100010305     C*
018200040802     C                   exsr      impvab
018300040802     C                   exsr      wrivab
018400020305     C                   exsr      wrivat                                       => carico VAT
018500040802     C*
018600010305     C                   endif
018700000905     C*
018800000905     C                   else
018900000905     C                   eval      vinflg = '1'
019000000905     C                   endif
019100000905     C*
019200000905     C  N70              update    tivin000
019300000905     C*
019400991022     C  N70              ENDdo
019500010202     C*
019600010202     C                   endif
019700990910
019800990910     C* Se non ci sono record con errori ...
019900000710     C                   if        §ctrno = 0
020000990910     C* ... restituisco esito OK.
020100990921     C                   eval      wrkesito = '0'
020200990910     C                   else
020300010201     C                   if        §ctrokvb > 0
020400990921     C                   eval      wrkesito = '1'
020500000710     C                   else
020600000710     C                   eval      wrkesito = '2'
020700990910     C                   endif
020800000710     C                   endif
020900990910     C*
021000990914     C                   if        %open(tivin00r)
021100990908     C                   close     tivin00r
021200990914     C                   endif
021300021113     C                   if        %open(fivabwwr)
021400021113     C                   close     fivabwwr
021500990914     C                   endif
021600021113     C                   if        %open(fivatwwr)
021700021113     C                   close     fivatwwr
021800010201     C                   endif
021900990910     C*
022000010201     C                   if        §ctrokvb > 0
022100000724     C                             and vlrpoi <> *zeros
022200010202     C                   exsr      invio
022300990920     C                   endif
022400990920     C*
022500910830     C                   ENDSR
022600000613     C***
022700010305
022800010305     C*----------------------------------------------------*
022900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
023000010305     C*----------------------------------------------------*
023100020305     C     WRIVAB        BEGSR
023200010305     C*
023300021113     C                   write     fivab000                                     => scarico il VAB
023400010305     C*
023500010305     C                   ENDSR
023600990920
023700000801     C*----------------------------------------------------*
023800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023900000801     C*----------------------------------------------------*
024000010201     C     INZVAR        BEGSR
024100000801     C*
024200040802     C                   Z-ADD     *zeros        Num5_0            5 0
024300040802     C                   MOVEL     '0'           FlgCAS            1
024400000801     C*
024500000801     C                   ENDSR
024600000801     C*----------------------------------------------------*
024700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024800000801     C*----------------------------------------------------*
024900000801     C     DEFCAM        BEGSR
025000000801     C*
025100021113     C                   CLEAR                   FIVAB000
025200040802     C                   CLEAR                   FIVAT000
025300020619     C* Imposto i valori di default...
025400040802     C                   Z-ADD     1200433       VABCCM
025500040802     C                   Z-ADD     1200433       VATCCM
025600040802     C                   Z-ADD     120           VABLNP
025700040802     C                   Z-ADD     120           VATLNP
025800040714     C                   Z-ADD     000           VABCTR
025900040714     C                   MOVEL     '7Q'          VABCTM
026000020619     C* ... e poi verifico se sono stati passati come parametri
026100020619     C                   IF        vlrppt > *blanks
026200040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABCCM
026700020619     C                   Z-ADD     PiVal         VATCCM
026800020619     C                   ENDIF
026900040506     C                   ENDIF
027000040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
027100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027200020619     C                   EXSR      CHKNUM
027300020619     C                   IF        PiInt=*on
027400020619     C                   Z-ADD     PiVal         VABLNP
027500020619     C                   Z-ADD     PiVal         VATLNP
027600040506     C                   ENDIF
027700020619     C                   ENDIF
027800040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028000020619     C                   EXSR      CHKNUM
028100020619     C                   IF        PiInt=*on
028200020619     C                   Z-ADD     PiVal         VABCTR
028300040506     C                   ENDIF
028400020619     C                   ENDIF
028500020619     C                   ENDIF
028600000801     C*
028700000801     C                   ENDSR
028800000801     C*----------------------------------------------------*
028900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029000000801     C*----------------------------------------------------*
029100010201     C     IMPVAB        BEGSR
029200000801     C*
029300020305     C                   EXSR      INZVAR
029400020305     C                   EXSR      DEFCAM
029500010305     C*
029600000801     C                   Z-ADD     *zeros        errore            1 0
029700000830     C                   MOVEL     datcor        VABAAS
029800020305     C                   MOVEL     datcor        VATAAS
029900040526     C                   MOVE      datcor        VABMGS
030000021113     C                   MOVE(P)   vlrpoi        VABFGS
030100021113     C                   MOVE(P)   vlrpoi        VATFGS
030200040802     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600040802     C                   EVAL      VABRD2=%trim(%subst(vindta:1+35:60-35))
030700040802     C                   EVAL      VABIND=%trim(%subst(vindta:61:35))
030800040802     C                   EVAL      VABLOD=%trim(%subst(vindta:101:25))
030900040802     C                   EVAL      VABPRD=%trim(%subst(vindta:126:2))
031000040802     C                   EVAL      VABRMA=%trim(%subst(vindta:169:6))
031100040802     C                   EVAL      VABNOT=%trim(%subst(vindta:175:35))
031200040802     C                   EVAL      VABNT2=%trim(%subst(vindta:175+35:56-35))
031300040506     C* CAD
031400040802     C                   EVAL      PiStr=%trim(%subst(vindta:96:5))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700010201     C                   Z-ADD     PiVal         Num5_0
031800040506     C                   MOVEL(P)  Num5_0        VABCAD
031900010201     C                   ELSE
032000040506     C                   ADD       1             errore
032100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032200040506     C                             + ' ' + 'VABCAD'
032300010201     C                   ENDIF
032400040506     C* Reperisco la provincia dal CAP e dalla località
032500040526     C                   IF        VABCAD <> *blanks AND
032600040526     C                             VABPRD  = *blanks
032700040506     C                   CLEAR                   TISI95DS
032800040506     C                   EVAL      I95TCN = '3'
032900040506     C                   Z-ADD     datcor        I95DAT
033000040506     C                   EVAL      I95CAP = VABCAD
033100040506     C                   EVAL      I95LOC = VABLOD
033200040506     C                   CALL      'TISI95R'
033300040506     C                   PARM                    TISI95DS
033400040506     C                   EVAL      VABPRD = O95PRV
033500040506     C                   ENDIF
033600040506     C* NCL
033700040802     C                   EVAL      PiStr=%trim(%subst(vindta:128:4))
033800040506     C                   EXSR      CHKNUM
033900040506     C                   IF        PiInt=*on
034000040506     C                   Z-ADD     PiVal         VABNCL
034100040506     C                   ELSE
034200040506     C                   ADD       1             errore
034300040506     C                   EVAL      vinmsg = %trimr(vinmsg)
034400040506     C                             + ' ' + 'VABNCL'
034500040506     C                   ENDIF
034600040506     C* PKB
034700040802     C                   EVAL      PiStr=%trim(%subst(vindta:132:7))
034800010201     C                   EXSR      CHKNUM
034900010201     C                   IF        PiNum=*on
035000011115     C                   Z-ADD     PiVal         VABPKB
035100010201     C                   ELSE
035200010201     C                   ADD       1             errore
035300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035400010201     C                             + ' ' + 'VABPKB'
035500010201     C                   ENDIF
035600040506     C* NSP/RMN
035700040802     C                   EVAL      PiStr=%trim(%subst(vindta:169:6))
035800010201     C                   EXSR      CHKNUM
035900010201     C                   IF        PiInt=*on
036000040506     C                   Z-ADD     PiVal         VABNSP
036100040506     C                   Z-ADD     PiVal         VATNSP
036200040506     C                   Z-ADD     PiVal         VABRMN
036300010201     C                   ELSE
036400010201     C                   ADD       1             errore
036500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036600040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036700040526     C                   ENDIF
036800040802     C* CAS
036900040802     C                   IF        %subst(vindta:144:25) <> *blanks
037000040802     C                   EVAL      FlgCAS = '1'
037100040802     C                   EVAL      PiStr=%trim(%subst(vindta:144:25))
037200040714     C                   EXSR      CHKNUM
037300040714     C                   IF        PiNum=*on
037400040802     C                   Z-ADD     PiVal         VABCAS
037500040802     C                   EVAL      VABVCA = 'EUR'
037600040714     C                   ELSE
037700040714     C                   ADD       1             errore
037800040714     C                   EVAL      vinmsg = %trimr(vinmsg)
037900040802     C                             + ' ' + 'VABCAS'
038000040714     C                   ENDIF
038100040802     C                   ENDIF
038200010205     C*
038300010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038400040802     C                   IF        FlgCAS <> '0'
038500010205     C                   EVAL      VABCBO = '4'
038600010205     C                   ELSE
038700010205     C                   EVAL      VABCBO = '1'
038800010205     C                   ENDIF
038900020305     C*
039000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039100011113     C                   EXSR      CHKIMPDIV
039200010202     C*
039300000801     C* Ebbene...
039400000801     C                   ADD       1             §CTRMO
039500010201     C                   IF        errore <> *zeros
039600000801     C                   ADD       1             §CTRNO
039700000801     C                   EVAL      recko = vindta
039800000801     C                   EXCEPT    rigadett
039900000801     C                   EVAL      vinflg = '2'
040000000801     C                   ELSE
040100010201     C                   ADD       1             §CTROKVB
040200000801     C                   ENDIF
040300000801     C*
040400000801     C                   ENDSR
040500010201     C*----------------------------------------------------*
040600040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040700010201     C*----------------------------------------------------*
040800020305     C     WRIVAT        BEGSR
040900040802     C*
041000040802     C* Inizializzo campi d wrk
041100040802     C                   Z-ADD     *zeros        wColloNum         4 0
041200040802     C                   MOVEL     *blanks       wColloAlfaDa      4
041300040802     C                   MOVE(P)   VABNCL        wColloAlfaA       4
041400040802     C*
041500040802     C* Valorizzo i campi chiave
041600040802     C                   EVAL      VATNSP = VABNSP
041700040802     C                   EVAL      VATTRC = 'E'
041800040802     C*
041900040802     C* Compongo i barcode
042000040802     C                   FOR       wColloNum = 1 to VABNCL
042100040802     C                   MOVE(P)   wColloNum     wColloAlfaDa
042200040802     C                   EVAL      VATNOT = '1200433' + %subst(vindta:169:6) +
042300040802     C                                      wColloAlfaDa + wColloAlfaA
042400040802     C                   WRITE     FIVAT000
042500040802     C                   ENDFOR
042600010201     C*
042700010201     C                   ENDSR
042800010202     C*----------------------------------------------------*
042900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043000010202     C*----------------------------------------------------*
043100020305     C     PREVAT        BEGSR
043200010202     C*
043300021113     C* Compongo il nome del membro da dare al FIVATWWR
043400010202     C                   eval      parmbr = vlrhdl
043500010202     C                   movel     'M'           parmbr
043600040802     C                   eval      parccm = '01200433'
043700010202     C                   eval      paropz = '1'
043800010202     C* Effettuo la chiamata al CLLE preposto
043900040506     C                   call(e)   'TITVVTC'
044000010202     C                   parm                    parccm
044100010202     C                   parm                    parmbr
044200010202     C                   parm                    paropz
044300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044400010202     C                   if        %error
044500010202     C                   movel     '1'           chkcall
044600010202     C                   else
044700010202     C                   movel     '0'           chkcall
044800010202     C                   endif
044900010202     C*
045000010202     C                   ENDSR
045100000801     C*----------------------------------------------------*
045200000801     C*  CONTROLLO NUMERICITA' CAMPI
045300000801     C*----------------------------------------------------*
045400000801     C     CHKNUM        BEGSR
045500000801     C*
045600000801     C                   call(e)   'ISNUMERIC'
045700000801     C                   PARM                    PiStr            30
045800040714     C                   PARM      ','           PiDecChr          1
045900000801     C                   PARM      *ZEROS        PiVal            30 9
046000000801     C                   PARM      '0'           PiInt             1
046100000801     C                   PARM      '0'           PiNum             1
046200000801     C                   IF        %error
046300000801     C                   EVAL      PiInt=*off
046400000801     C                   ENDIF
046500000801     C*
046600000801     C                   ENDSR
046700000801     C***
046800000801
046900011113
047000011113     C*----------------------------------------------------*
047100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047200011113     C*----------------------------------------------------*
047300011113     C     CHKIMPDIV     BEGSR
047400011113     C*
047500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047600011113     C                   Z-ADD     *zeros        wrkDec            9 9
047700011113     C*
047800011113     C* Come prima cosa effettuo considerazioni sulla divisa
047900011113     C                   IF        vabIAS > *zeros
048000011113     C                   IF        vabVAS <> 'EUR'
048100011113     C                   EVAL      vabVAS =  'ITL'
048200011113     C                   ENDIF
048300011113     C                   ENDIF
048400011113     C*
048500011113     C                   IF        vabCAS > *zeros
048600011113     C                   IF        vabVCA <> 'EUR'
048700011113     C                   EVAL      vabVCA =  'ITL'
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C                   IF        vabVMD > *zeros
049200020305     C                   IF        vabVAD <> 'EUR'
049300011113     C                   EVAL      vabVAD =  'ITL'
049400011113     C                   ENDIF
049500011113     C                   ENDIF
049600011113     C*
049700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049800011113     C                   Z-ADD     vabIAS        wrkDec
049900011113     C                   IF        wrkDec > *zeros
050000011113     C                   IF        vabVAS = 'ITL'
050100011113     C                   EVAL      vabIAS = *zeros
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050600011113     C                   Z-ADD     vabCAS        wrkDec
050700011113     C                   IF        wrkDec > *zeros
050800011113     C                   IF        vabVCA = 'ITL'
050900011113     C                   EVAL      vabCAS = *zeros
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051400011113     C                   Z-ADD     vabVMD        wrkDec
051500011113     C                   IF        wrkDec > *zeros
051600011113     C                   IF        vabVAD = 'ITL'
051700011113     C                   EVAL      vabVMD = *zeros
051800011113     C                   ENDIF
051900011113     C                   ENDIF
052000011113     C*
052100011113     C                   ENDSR
052200011113     C***
052300011113
052400011113
052500000801
052600000801
052700990920      /TITLE Invio dei dati al punto operativo.
052800010202     C     invio         BEGSR
052900990920     C*
053000021113     C* 1° invio FIVAT
053100010201     C                   reset                   dscmz
053200010201     C                   move      vlrpoi        cmzdst
053300021113     C                   eval      cmzfld = 'FIVATWWR'
053400010201     C                   eval      cmzmbd = vlrhdl
053500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053600021009     C***                if        prmfir = *blanks
053700021113     C                   eval      cmzfla = 'FIVAT00F'
053800021113     C                   eval      cmzmba = 'FIVAT00F'
053900021009     C***                else
054000021009     C***                eval      cmzfla = prmfir
054100021009     C***                eval      cmzmba = prmfir
054200021009     C***                endif
054300010201     C                   eval      cmznrr = *zeros
054400020305     C                   move      §ctrokvt      cmznrr
054500021018     C                   eval      cmzlba = vlrfl1
054600010201     C                   call(e)   'TIS711C'
054700010201     C                   parm                    dscmz
054800010201     C                   parm      *blanks       esito
054900010205     C                   if        %error
055000010205     C                             or cmzerr = '1'
055100010205     C                             or esito  = '1'
055200010205     C                   eval      wrkesito = '3'
055300010205     C                   else
055400010201     C*
055500021113     C* 2° invio FIVAB
055600010201     C                   reset                   dscmz
055700010201     C                   move      vlrpoi        cmzdst
055800010201     C                   eval      cmzfld = vlrfou
055900010201     C                   eval      cmzmbd = vlrhdl
056000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056100021009     C***                if        prmfir = *blanks
056200021113     C                   eval      cmzfla = 'FIVAB00F'
056300021113     C                   eval      cmzmba = 'FIVAB00F'
056400021009     C***                else
056500021009     C***                eval      cmzfla = prmfir
056600021009     C***                eval      cmzmba = prmfir
056700021009     C***                endif
056800010201     C                   eval      cmznrr = *zeros
056900010201     C                   move      §ctrokvb      cmznrr
057000021018     C                   eval      cmzlba = vlrfl1
057100010201     C                   call(e)   'TIS711C'
057200010201     C                   parm                    dscmz
057300010201     C                   parm      *blanks       esito
057400010201     C                   if        %error
057500010201     C                             or cmzerr = '1'
057600010201     C                             or esito  = '1'
057700010201     C                   eval      wrkesito = '3'
057800010201     C                   endif
057900010205     C                   endif
058000990920     C*
058100000613     C                   ENDSR
058200000613     C***
058300070411
058400070411     C     *pssr         BEGSR
058500070411     C*
058600070411     C                   if        %open(tivin00r)
058700070411     C                   close     tivin00r
058800070411     C                   endif
058900070411     C                   if        %open(fivabwwr)
059000070411     C                   close     fivabwwr
059100070411     C                   endif
059200070411     C                   if        %open(fivatwwr)
059300070411     C                   close     fivatwwr
059400070411     C                   endif
059500070411     C*
059600070411     C* Effettuo la chiamata al CLLE preposto
059700070411     C                   call(e)   'TITVVTC'
059800070411     C                   parm                    parccm
059900070411     C                   parm                    parmbr
060000070411     C                   parm      '2'           paropz
060100070411     C*
060200070411     C                   eval      wrkesito = '2'
060300070411     C*
060400070411     C                   seton                                        LR
060500070411     C*
060600070411     C                   ENDSR     '*CANCL'
060700070411     C***
060800070411
060900990910
061000000613     C     *inzsr        BEGSR
061100990910     C*
061200990910     C     *entry        plist
061300990920     C                   parm                    tivlrds
061400990921     C                   parm      wrkesito      esito
061500000724     C                   parm                    prmlit
061600000710     C                   parm                    prmfir
061700000613     C*
061800000830     C* CALCOLA LA DATA CORRENTE
061900000830     C                   time                    wn14             14 0
062000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062100000830     C                   z-add     wn8           g08dat
062200000830     C                   z-add     *zeros        g08inv
062300000830     C                   movel     '0'           g08err
062400000830     C                   call      'XSRDA8'
062500000830     C                   parm                    wlbda8
062600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062700000830     C*
062800000613     C                   ENDSR
062900000613     C***
063000990908
063100040802     Otitve3p   E            riepilogo         2
063200990915     O                                              'Upload via Internet'
063300990915     O                                           +1 'Traduzione TIVIN00R -'
063400021113     O                                           +1 'FIVABWWR/FIVATWWR'
063500010201     O                                           +1 'Report testate bolle'
063600990915     O          E            riepilogo   2
063700990915     O                       wrkdata
063800990915     O                       wrkora              +1
063900990915     O                       procname            +1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Cliente..................:'
064200990915     O                       VABCCM        z     +1
064300990915     O          E            riepilogo   2
064400990920     O                                              'Riferimento Strategi.....:'
064500990920     O                       vlrhdl              +1
064600990915     O          E            riepilogo   2
064700990915     O                                              'Giusti...................:'
064800010201     O                       §CTROKVB      2   +  1
064900990915     O          E            riepilogo   2
065000990915     O                                              'Sbagliati e corretti.....:'
065100971022     O                       §CTRMO        2   +  1
065200990915     O          E            riepilogo   2
065300990915     O                                              'Sbagliati e scartati.....:'
065400971022     O                       §CTRNO        2   +  1
065500000613
065600040802     Otitve3ps  E            testdett          2
065700000613     O                                              'Upload via Internet'
065800000613     O                                           +1 'Traduzione TIVIN00R -'
065900021113     O                                           +1 'FIVABWWR/FIVATWWR'
066000010201     O                                           +1 'Report testate bolle'
066100000616     O          E            testdett    3
066200000613     O                                           +2 'N° rec'
066300000613     O                                           +3 'Anteprima contenuto'
066400000616     O          E            rigadett    2
066500000613     O                       rrnum               +2
066600000621     O                       recko               +3
066700000616     O          E            findett     2
066800000613     O                       wrkdata
066900000613     O                       wrkora              +1
067000000613     O                       procname            +1
067100000616     O          E            findett     2
067200000613     O                                              'Cliente..................:'
067300000613     O                       VABCCM        z     +1
067400000616     O          E            findett     2
067500000613     O                                              'Riferimento Strategi.....:'
067600000613     O                       vlrhdl              +1
067700000616     O          E            findett     2
067800000613     O                                              'Giusti...................:'
067900010201     O                       §CTROKVB      2   +  1
068000000616     O          E            findett     2
068100000613     O                                              'Sbagliati e corretti.....:'
068200000613     O                       §CTRMO        2   +  1
068300000616     O          E            findett     2
068400000613     O                                              'Sbagliati e scartati.....:'
068500000613     O                       §CTRNO        2   +  1
068600000512** CMD - COMANDI CL
068700040802OVRPRTF FILE(TITVE3P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068800040802OVRPRTF FILE(TITVE3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068900040802DLTOVR FILE(TITVE3P TITVE3PS) LVL(*)
069000000512
069100000512
