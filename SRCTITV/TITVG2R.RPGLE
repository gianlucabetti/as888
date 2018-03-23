000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800041201     Ftitvg2p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100041201     Ftitvg2ps  O    f  198        PRINTER usropn
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
003500010202     D parccm          s              8    INZ(*blanks)
003600010202     D parmbr          s             10    INZ(*blanks)
003700010202     D paropz          s              1    INZ(*blanks)
003800010202     D chkcall         s              1    INZ(*blanks)
003900000830
004000000830     D*------------------
004100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200000830     D*------------------
004300000830     D WLBDA8          DS                  INZ
004400000830     D  G08DAT                 1      8  0
004500000830     D  G08INV                 9     16  0
004600000830     D  G08ERR                17     17
004700000830     D  G08TGI                18     22  0
004800000830     D*
004900990908
005000010201
005100010201
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700000724     C*
005800000724     C* SE OCCORRE SPEDIRE IN FILIALE
005900000724     C                   if        vlrpoi <> *zeros
006000000724     C*
006100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006300000724     C     vlrpoi        chain     azorg01l
006400000724     C                   if        %found
006500000616     C                   movel(p)  CMD(1)        depcmd
006600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006700000616     C*
006800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006900011113     C                   Z-ADD     150           LENGH            15 5
007000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007100000616     C                   PARM                    depcmd
007200000616     C                   PARM                    LENGH
007300000724     C*
007400000724     C                   endif
007500000724     C                   endif
007600000616     C*
007700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007800000616     C                   movel(p)  CMD(2)        depcmd
007900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000616     C*
008700041201     C                   if        not %open(titvg2ps)
008800041201     C                   open      titvg2ps
008900000616     C                   except    testdett
009000000613     C                   endif
009100000613     C*
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009400000613     C*
009500010202     C* Effettuo la chiamata al CLLE preposto
009600040506     C                   call(e)   'TITVVTC'
009700010202     C                   parm                    parccm
009800010202     C                   parm                    parmbr
009900010202     C                   parm      '2'           paropz
010000010202     C*
010100041201     C                   if        %open(titvg2ps)
010200000616     C                   except    findett
010300041201     C                   close     titvg2ps
010400000613     C                   endif
010500000616     C*
010600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    CMD(3)
011000000616     C                   PARM                    LENGH
011100000616     C*
011200000801     C
011300010201     C                   seton                                        LR
011400000613
011500000613
011600000613     C*--------------------------------------------------------
011700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011800000613     C*--------------------------------------------------------
011900000621     C     STPR          BEGSR
012000000613     C*
012100041201     C                   if        not %open(titvg2p)
012200041201     C                   open      titvg2p
012300990915     C                   endif
012400990915     C*
012500990915     C                   except    riepilogo
012600990915     C*
012700041201     C                   if        %open(titvg2p)
012800041201     C                   close     titvg2p
012900990914     C                   endif
013000990910     C*
013100000613     C                   ENDSR
013200000613     C***
013300990908
013400000801
013500910830     C*--------------------------------------------------------
013600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
013700910830     C*--------------------------------------------------------
013800040526     C     RWFILE        BEGSR
013900990910     C*
014000990914     C                   if        not %open(tivin00r)
014100990908     C                   open      tivin00r
014200990914     C                   endif
014300021113     C                   if        not %open(fivabwwr)
014400021113     C                   open      fivabwwr
014500990914     C                   endif
014600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014700020305     C                   exsr      prevat
014800010201     C*
014900010202     C                   if        chkcall = '0'
015000010202     C*
015100021113     C                   if        not %open(fivatwwr)
015200021113     C                   open      fivatwwr
015300010201     C                   endif
015400990910     C*
015500010201     C                   clear                   §CTROKVB          5 0
015600020305     C                   clear                   §CTROKVT          5 0
015700000801     C                   clear                   §CTRMO            5 0
015800000801     C                   clear                   §CTRNO            5 0
015900990910     C*
016000921023     C                   DO        *HIVAL
016100990913     C*
016200990915     C                   READ      tivin00r                               70
016300000905     C                   if        vindta > *blanks
016400000613     C                   add       1             rrnum
016500000801     C*
016600000801     C                   if        *in70 = *off
016700000801     C                             and
016800000801     C                             (vinflg = *blanks
016900000801     C                              or vinflg = '0'
017000000801     C                              or vinflg = '2')
017100000801     C*
017200000801     C                   clear                   vinmsg
017300000801     C                   eval      vinflg = '1'
017400010305     C*
017500040802     C                   exsr      impvab
017600040802     C                   exsr      wrivab
017700020305     C                   exsr      wrivat                                       => carico VAT
017800040802     C*
017900010305     C                   endif
018000000905     C*
018100000905     C                   else
018200000905     C                   eval      vinflg = '1'
018300000905     C                   endif
018400000905     C*
018500000905     C  N70              update    tivin000
018600000905     C*
018700991022     C  N70              ENDdo
018800010202     C*
018900010202     C                   endif
019000990910
019100990910     C* Se non ci sono record con errori ...
019200000710     C                   if        §ctrno = 0
019300990910     C* ... restituisco esito OK.
019400990921     C                   eval      wrkesito = '0'
019500990910     C                   else
019600010201     C                   if        §ctrokvb > 0
019700990921     C                   eval      wrkesito = '1'
019800000710     C                   else
019900000710     C                   eval      wrkesito = '2'
020000990910     C                   endif
020100000710     C                   endif
020200990910     C*
020300990914     C                   if        %open(tivin00r)
020400990908     C                   close     tivin00r
020500990914     C                   endif
020600021113     C                   if        %open(fivabwwr)
020700021113     C                   close     fivabwwr
020800990914     C                   endif
020900021113     C                   if        %open(fivatwwr)
021000021113     C                   close     fivatwwr
021100010201     C                   endif
021200990910     C*
021300010201     C                   if        §ctrokvb > 0
021400000724     C                             and vlrpoi <> *zeros
021500010202     C                   exsr      invio
021600990920     C                   endif
021700990920     C*
021800910830     C                   ENDSR
021900000613     C***
022000010305
022100010305     C*----------------------------------------------------*
022200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022300010305     C*----------------------------------------------------*
022400020305     C     WRIVAB        BEGSR
022500010305     C*
022600021113     C                   write     fivab000                                     => scarico il VAB
022700010305     C*
022800010305     C                   ENDSR
022900990920
023000000801     C*----------------------------------------------------*
023100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023200000801     C*----------------------------------------------------*
023300010201     C     INZVAR        BEGSR
023400000801     C*
023500040802     C                   Z-ADD     *zeros        Num5_0            5 0
023600040802     C                   MOVEL     '0'           FlgCAS            1
023700000801     C*
023800000801     C                   ENDSR
023900000801     C*----------------------------------------------------*
024000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024100000801     C*----------------------------------------------------*
024200000801     C     DEFCAM        BEGSR
024300000801     C*
024400021113     C                   CLEAR                   FIVAB000
024500040802     C                   CLEAR                   FIVAT000
024600020619     C* Imposto i valori di default...
024700041201     C                   Z-ADD     2770756       VABCCM
024800041201     C                   Z-ADD     2770756       VATCCM
024900041201     C                   Z-ADD     043           VABLNP
025000041201     C                   Z-ADD     043           VATLNP
025100040714     C                   Z-ADD     000           VABCTR
025200020619     C* ... e poi verifico se sono stati passati come parametri
025300020619     C                   IF        vlrppt > *blanks
025400040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025600020619     C                   EXSR      CHKNUM
025700020619     C                   IF        PiInt=*on
025800020619     C                   Z-ADD     PiVal         VABCCM
025900020619     C                   Z-ADD     PiVal         VATCCM
026000020619     C                   ENDIF
026100040506     C                   ENDIF
026200040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABLNP
026700020619     C                   Z-ADD     PiVal         VATLNP
026800040506     C                   ENDIF
026900020619     C                   ENDIF
027000040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027100020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027200020619     C                   EXSR      CHKNUM
027300020619     C                   IF        PiInt=*on
027400020619     C                   Z-ADD     PiVal         VABCTR
027500040506     C                   ENDIF
027600020619     C                   ENDIF
027700020619     C                   ENDIF
027800000801     C*
027900000801     C                   ENDSR
028000000801     C*----------------------------------------------------*
028100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028200000801     C*----------------------------------------------------*
028300010201     C     IMPVAB        BEGSR
028400000801     C*
028500020305     C                   EXSR      INZVAR
028600020305     C                   EXSR      DEFCAM
028700010305     C*
028800000801     C                   Z-ADD     *zeros        errore            1 0
028900000830     C                   MOVEL     datcor        VABAAS
029000020305     C                   MOVEL     datcor        VATAAS
029100040526     C                   MOVE      datcor        VABMGS
029200021113     C                   MOVE(P)   vlrpoi        VABFGS
029300021113     C                   MOVE(P)   vlrpoi        VATFGS
029400041201     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
029500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029700020117     C* ==
029800041201     C                   EVAL      VABRD2=%trim(%subst(vindta:52:5))
029900041201     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
030000041201     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
030100041201     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
030200041201     C                   EVAL      VABNAS=%trim(%subst(vindta:145:15))
030300041201     C                   EVAL      VABRMA=%trim(%subst(vindta:319:10))
030400041201     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
030500041201     C                   EVAL      VABNT2=%trim(%subst(vindta:232:35))
030600041201     C                   IF        %subst(vindta:14:1) = '1'
030700041201     C                   EVAL      VABFFD = 'S'
030800041201     C                   ENDIF
030900040506     C* CAD
031000041201     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
031100010201     C                   EXSR      CHKNUM
031200010201     C                   IF        PiInt=*on
031300010201     C                   Z-ADD     PiVal         Num5_0
031400040506     C                   MOVEL(P)  Num5_0        VABCAD
031500010201     C                   ELSE
031600040506     C                   ADD       1             errore
031700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031800040506     C                             + ' ' + 'VABCAD'
031900010201     C                   ENDIF
032000041201     C* NCL
032100041201     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
032200040506     C                   EXSR      CHKNUM
032300040506     C                   IF        PiInt=*on
032400040506     C                   Z-ADD     PiVal         VABNCL
032500040506     C                   ELSE
032600040506     C                   ADD       1             errore
032700040506     C                   EVAL      vinmsg = %trimr(vinmsg)
032800040506     C                             + ' ' + 'VABNCL'
032900040506     C                   ENDIF
033000040506     C* PKB
033100041201     C                   EVAL      PiStr=%trim(%subst(vindta:170:4))
033200010201     C                   EXSR      CHKNUM
033300010201     C                   IF        PiNum=*on
033400011115     C                   Z-ADD     PiVal         VABPKB
033500010201     C                   ELSE
033600010201     C                   ADD       1             errore
033700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
033800010201     C                             + ' ' + 'VABPKB'
033900010201     C                   ENDIF
034000041201     C* VLB
034100041201     C                   EVAL      PiStr=%trim(%subst(vindta:296:7))
034200041201     C                   EXSR      CHKNUM
034300041201     C                   IF        PiNum=*on
034400041201     C                   EVAL      VABVLB = PiVal / 100                         * gestisco 2 dec.
034500041201     C                   ELSE
034600041201     C                   ADD       1             errore
034700041201     C                   EVAL      vinmsg = %trimr(vinmsg)
034800041201     C                             + ' ' + 'VABVLB'
034900041201     C                   ENDIF
035000040506     C* NSP/RMN
035100041201     C                   EVAL      PiStr=%trim(%subst(vindta:189:8))
035200010201     C                   EXSR      CHKNUM
035300010201     C                   IF        PiInt=*on
035400040506     C                   Z-ADD     PiVal         VABNSP
035500040506     C                   Z-ADD     PiVal         VATNSP
035600040506     C                   Z-ADD     PiVal         VABRMN
035700010201     C                   ELSE
035800010201     C                   ADD       1             errore
035900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
036000040506     C                             + ' ' + 'VABNSP VATNSP VABRMN'
036100040526     C                   ENDIF
036200041201     C* CAS
036300041206     C                   IF        %trim(%subst(vindta:277:9)) <> *zeros
036400040802     C                   EVAL      FlgCAS = '1'
036500041201     C                   EVAL      PiStr=%trim(%subst(vindta:277:9))
036600040714     C                   EXSR      CHKNUM
036700040714     C                   IF        PiNum=*on
036800041201     C                   EVAL      VABCAS = PiVal
036900040802     C                   EVAL      VABVCA = 'EUR'
037000040714     C                   ELSE
037100040714     C                   ADD       1             errore
037200040714     C                   EVAL      vinmsg = %trimr(vinmsg)
037300040802     C                             + ' ' + 'VABCAS'
037400040714     C                   ENDIF
037500040802     C                   ENDIF
037600041201     C* IAS
037700041201     C                   EVAL      PiStr=%trim(%subst(vindta:286:9))
037800041201     C                   EXSR      CHKNUM
037900041201     C                   IF        PiNum=*on
038000041201     C                   EVAL      VABIAS = PiVal
038100041201     C                   EVAL      VABVAS = 'EUR'
038200041201     C                   ELSE
038300041201     C                   ADD       1             errore
038400041201     C                   EVAL      vinmsg = %trimr(vinmsg)
038500041201     C                             + ' ' + 'VABIAS'
038600041201     C                   ENDIF
038700010205     C*
038800010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
038900040802     C                   IF        FlgCAS <> '0'
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
041000010201     C*----------------------------------------------------*
041100040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041200010201     C*----------------------------------------------------*
041300020305     C     WRIVAT        BEGSR
041400040802     C*
041500040802     C* Valorizzo i campi chiave
041600040802     C                   EVAL      VATNSP = VABNSP
041700041201     C                   EVAL      VATTRC = 'B'
041800040802     C*
041900041201     C* Compongo i barcode e scarico il buffer del FIVAT
042000041201     C                   EVAL      VATNOT = %subst(vindta:92:15)
042100040802     C                   WRITE     FIVAT000
042200010201     C*
042300010201     C                   ENDSR
042400010202     C*----------------------------------------------------*
042500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042600010202     C*----------------------------------------------------*
042700020305     C     PREVAT        BEGSR
042800010202     C*
042900021113     C* Compongo il nome del membro da dare al FIVATWWR
043000010202     C                   eval      parmbr = vlrhdl
043100010202     C                   movel     'M'           parmbr
043200041201     C                   eval      parccm = vlrksc
043300010202     C                   eval      paropz = '1'
043400010202     C* Effettuo la chiamata al CLLE preposto
043500040506     C                   call(e)   'TITVVTC'
043600010202     C                   parm                    parccm
043700010202     C                   parm                    parmbr
043800010202     C                   parm                    paropz
043900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044000010202     C                   if        %error
044100010202     C                   movel     '1'           chkcall
044200010202     C                   else
044300010202     C                   movel     '0'           chkcall
044400010202     C                   endif
044500010202     C*
044600010202     C                   ENDSR
044700000801     C*----------------------------------------------------*
044800000801     C*  CONTROLLO NUMERICITA' CAMPI
044900000801     C*----------------------------------------------------*
045000000801     C     CHKNUM        BEGSR
045100000801     C*
045200000801     C                   call(e)   'ISNUMERIC'
045300000801     C                   PARM                    PiStr            30
045400040714     C                   PARM      ','           PiDecChr          1
045500000801     C                   PARM      *ZEROS        PiVal            30 9
045600000801     C                   PARM      '0'           PiInt             1
045700000801     C                   PARM      '0'           PiNum             1
045800000801     C                   IF        %error
045900000801     C                   EVAL      PiInt=*off
046000000801     C                   ENDIF
046100000801     C*
046200000801     C                   ENDSR
046300000801     C***
046400000801
046500011113
046600011113     C*----------------------------------------------------*
046700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046800011113     C*----------------------------------------------------*
046900011113     C     CHKIMPDIV     BEGSR
047000011113     C*
047100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047200011113     C                   Z-ADD     *zeros        wrkDec            9 9
047300011113     C*
047400011113     C* Come prima cosa effettuo considerazioni sulla divisa
047500011113     C                   IF        vabIAS > *zeros
047600011113     C                   IF        vabVAS <> 'EUR'
047700011113     C                   EVAL      vabVAS =  'ITL'
047800011113     C                   ENDIF
047900011113     C                   ENDIF
048000011113     C*
048100011113     C                   IF        vabCAS > *zeros
048200011113     C                   IF        vabVCA <> 'EUR'
048300011113     C                   EVAL      vabVCA =  'ITL'
048400011113     C                   ENDIF
048500011113     C                   ENDIF
048600011113     C*
048700011113     C                   IF        vabVMD > *zeros
048800020305     C                   IF        vabVAD <> 'EUR'
048900011113     C                   EVAL      vabVAD =  'ITL'
049000011113     C                   ENDIF
049100011113     C                   ENDIF
049200011113     C*
049300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049400011113     C                   Z-ADD     vabIAS        wrkDec
049500011113     C                   IF        wrkDec > *zeros
049600011113     C                   IF        vabVAS = 'ITL'
049700011113     C                   EVAL      vabIAS = *zeros
049800011113     C                   ENDIF
049900011113     C                   ENDIF
050000011113     C*
050100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
050200011113     C                   Z-ADD     vabCAS        wrkDec
050300011113     C                   IF        wrkDec > *zeros
050400011113     C                   IF        vabVCA = 'ITL'
050500011113     C                   EVAL      vabCAS = *zeros
050600011113     C                   ENDIF
050700011113     C                   ENDIF
050800011113     C*
050900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051000011113     C                   Z-ADD     vabVMD        wrkDec
051100011113     C                   IF        wrkDec > *zeros
051200011113     C                   IF        vabVAD = 'ITL'
051300011113     C                   EVAL      vabVMD = *zeros
051400011113     C                   ENDIF
051500011113     C                   ENDIF
051600011113     C*
051700011113     C                   ENDSR
051800011113     C***
051900011113
052000011113
052100000801
052200000801
052300990920      /TITLE Invio dei dati al punto operativo.
052400010202     C     invio         BEGSR
052500990920     C*
052600021113     C* 1° invio FIVAT
052700010201     C                   reset                   dscmz
052800010201     C                   move      vlrpoi        cmzdst
052900021113     C                   eval      cmzfld = 'FIVATWWR'
053000010201     C                   eval      cmzmbd = vlrhdl
053100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053200021009     C***                if        prmfir = *blanks
053300021113     C                   eval      cmzfla = 'FIVAT00F'
053400021113     C                   eval      cmzmba = 'FIVAT00F'
053500021009     C***                else
053600021009     C***                eval      cmzfla = prmfir
053700021009     C***                eval      cmzmba = prmfir
053800021009     C***                endif
053900010201     C                   eval      cmznrr = *zeros
054000020305     C                   move      §ctrokvt      cmznrr
054100021018     C                   eval      cmzlba = vlrfl1
054200010201     C                   call(e)   'TIS711C'
054300010201     C                   parm                    dscmz
054400010201     C                   parm      *blanks       esito
054500010205     C                   if        %error
054600010205     C                             or cmzerr = '1'
054700010205     C                             or esito  = '1'
054800010205     C                   eval      wrkesito = '3'
054900010205     C                   else
055000010201     C*
055100021113     C* 2° invio FIVAB
055200010201     C                   reset                   dscmz
055300010201     C                   move      vlrpoi        cmzdst
055400010201     C                   eval      cmzfld = vlrfou
055500010201     C                   eval      cmzmbd = vlrhdl
055600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055700021009     C***                if        prmfir = *blanks
055800021113     C                   eval      cmzfla = 'FIVAB00F'
055900021113     C                   eval      cmzmba = 'FIVAB00F'
056000021009     C***                else
056100021009     C***                eval      cmzfla = prmfir
056200021009     C***                eval      cmzmba = prmfir
056300021009     C***                endif
056400010201     C                   eval      cmznrr = *zeros
056500010201     C                   move      §ctrokvb      cmznrr
056600021018     C                   eval      cmzlba = vlrfl1
056700010201     C                   call(e)   'TIS711C'
056800010201     C                   parm                    dscmz
056900010201     C                   parm      *blanks       esito
057000010201     C                   if        %error
057100010201     C                             or cmzerr = '1'
057200010201     C                             or esito  = '1'
057300010201     C                   eval      wrkesito = '3'
057400010201     C                   endif
057500010205     C                   endif
057600990920     C*
057700000613     C                   ENDSR
057800000613     C***
057900070411
058000070411     C     *pssr         BEGSR
058100070411     C*
058200070411     C                   if        %open(tivin00r)
058300070411     C                   close     tivin00r
058400070411     C                   endif
058500070411     C                   if        %open(fivabwwr)
058600070411     C                   close     fivabwwr
058700070411     C                   endif
058800070411     C                   if        %open(fivatwwr)
058900070411     C                   close     fivatwwr
059000070411     C                   endif
059100070411     C*
059200070411     C* Effettuo la chiamata al CLLE preposto
059300070411     C                   call(e)   'TITVVTC'
059400070411     C                   parm                    parccm
059500070411     C                   parm                    parmbr
059600070411     C                   parm      '2'           paropz
059700070411     C*
059800070411     C                   eval      wrkesito = '2'
059900070411     C*
060000070411     C                   seton                                        LR
060100070411     C*
060200070411     C                   ENDSR     '*CANCL'
060300070411     C***
060400070411
060500990910
060600000613     C     *inzsr        BEGSR
060700990910     C*
060800990910     C     *entry        plist
060900990920     C                   parm                    tivlrds
061000990921     C                   parm      wrkesito      esito
061100000724     C                   parm                    prmlit
061200000710     C                   parm                    prmfir
061300000613     C*
061400000830     C* CALCOLA LA DATA CORRENTE
061500000830     C                   time                    wn14             14 0
061600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061700000830     C                   z-add     wn8           g08dat
061800000830     C                   z-add     *zeros        g08inv
061900000830     C                   movel     '0'           g08err
062000000830     C                   call      'XSRDA8'
062100000830     C                   parm                    wlbda8
062200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
062300000830     C*
062400000613     C                   ENDSR
062500000613     C***
062600990908
062700041201     Otitvg2p   E            riepilogo         2
062800990915     O                                              'Upload via Internet'
062900990915     O                                           +1 'Traduzione TIVIN00R -'
063000021113     O                                           +1 'FIVABWWR/FIVATWWR'
063100010201     O                                           +1 'Report testate bolle'
063200990915     O          E            riepilogo   2
063300990915     O                       wrkdata
063400990915     O                       wrkora              +1
063500990915     O                       procname            +1
063600990915     O          E            riepilogo   2
063700990915     O                                              'Cliente..................:'
063800990915     O                       VABCCM        z     +1
063900990915     O          E            riepilogo   2
064000990920     O                                              'Riferimento Strategi.....:'
064100990920     O                       vlrhdl              +1
064200990915     O          E            riepilogo   2
064300990915     O                                              'Giusti...................:'
064400010201     O                       §CTROKVB      2   +  1
064500990915     O          E            riepilogo   2
064600990915     O                                              'Sbagliati e corretti.....:'
064700971022     O                       §CTRMO        2   +  1
064800990915     O          E            riepilogo   2
064900990915     O                                              'Sbagliati e scartati.....:'
065000971022     O                       §CTRNO        2   +  1
065100000613
065200041201     Otitvg2ps  E            testdett          2
065300000613     O                                              'Upload via Internet'
065400000613     O                                           +1 'Traduzione TIVIN00R -'
065500021113     O                                           +1 'FIVABWWR/FIVATWWR'
065600010201     O                                           +1 'Report testate bolle'
065700000616     O          E            testdett    3
065800000613     O                                           +2 'N° rec'
065900000613     O                                           +3 'Anteprima contenuto'
066000000616     O          E            rigadett    2
066100000613     O                       rrnum               +2
066200000621     O                       recko               +3
066300000616     O          E            findett     2
066400000613     O                       wrkdata
066500000613     O                       wrkora              +1
066600000613     O                       procname            +1
066700000616     O          E            findett     2
066800000613     O                                              'Cliente..................:'
066900000613     O                       VABCCM        z     +1
067000000616     O          E            findett     2
067100000613     O                                              'Riferimento Strategi.....:'
067200000613     O                       vlrhdl              +1
067300000616     O          E            findett     2
067400000613     O                                              'Giusti...................:'
067500010201     O                       §CTROKVB      2   +  1
067600000616     O          E            findett     2
067700000613     O                                              'Sbagliati e corretti.....:'
067800000613     O                       §CTRMO        2   +  1
067900000616     O          E            findett     2
068000000613     O                                              'Sbagliati e scartati.....:'
068100000613     O                       §CTRNO        2   +  1
068200000512** CMD - COMANDI CL
068300041201OVRPRTF FILE(TITVG2P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
068400041201OVRPRTF FILE(TITVG2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068500041201DLTOVR FILE(TITVG2P TITVG2PS) LVL(*)
068600000512
068700000512
