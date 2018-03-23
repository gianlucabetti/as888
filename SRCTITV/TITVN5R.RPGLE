000100050802      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050802     FEDIVABwr  O    E             DISK    usropn
000700050802     FEDIVATwr  O    E             DISK    usropn
000800050802     Ftitvn5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070502     F                                     infsr(*pssr)
001100050802     Ftitvn5ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070502     F                                     infsr(*pssr)
001400000512     D*------------
001500000512     D* COMANDI
001600000512     D*------------
001700011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001800000801     D*----------------------------------------------------
001900000801     D* DICHIARAZIOINE VARIABILI DI WRK
002000000801     D*----------------------------------------------------
002100990920     D dscmz         e ds                  inz
002200990910     D psds           sds
002300990910     D  procname         *PROC
002400990920     D tivlrds       e ds                  extname(tivlr00f)
002500990910     D esito           s              1
002600000724     D prmlit          s             10
002700000710     D prmfir          s             10
002800990921     D wrkesito        s                   like(esito)
002900990915     D wrkdata         s               d
003000990915     D wrkora          s               t
003100000613     D rrnum           s              6  0 INZ(*zeros)
003200000621     D recko           s            150    INZ(*blanks)
003300011113     D depcmd          s            150    INZ(*blanks)
003400040428     D depspe          s              8    INZ(*blanks)
003500040428     D curspe          s              8    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000010213
004100000801     D  Num5_0         s              5  0
004200030107     D  Num7_0         s              7  0
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
005400010201
005500010201
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100000724     C*
006200000724     C* SE OCCORRE SPEDIRE IN FILIALE
006300021017     C                   if        vlrpoi <> *zeros
006400000724     C*
006500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006700021017     C     vlrpoi        chain     azorg01l
006800000724     C                   if        %found
006900000616     C                   movel(p)  CMD(1)        depcmd
007000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007100000616     C*
007200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007300011113     C                   Z-ADD     150           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000616     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000724     C*
007800000724     C                   endif
007900000724     C                   endif
008000000616     C*
008100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200000616     C                   movel(p)  CMD(2)        depcmd
008300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008400000616     C*
008500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008600011113     C                   Z-ADD     150           LENGH            15 5
008700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008800000616     C                   PARM                    depcmd
008900000616     C                   PARM                    LENGH
009000000616     C*
009100050802     C                   if        not %open(titvn5ps)
009200050802     C                   open      titvn5ps
009300000616     C                   except    testdett
009400000613     C                   endif
009500000613     C*
009600050802     C                   EXSR      RWVAB                                        LETT/SCR. VAB/VAT
009700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009800000613     C*
009900010202     C* Effettuo la chiamata al CLLE preposto
010000050802     C                   call(e)   'TITVEVTC'
010100010202     C                   parm                    parccm
010200010202     C                   parm                    parmbr
010300010202     C                   parm      '2'           paropz
010400010202     C*
010500050802     C                   if        %open(titvn5ps)
010600000616     C                   except    findett
010700050802     C                   close     titvn5ps
010800000613     C                   endif
010900000616     C*
011000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    CMD(3)
011400000616     C                   PARM                    LENGH
011500000616     C*
011600000801     C
011700010201     C                   seton                                        LR
011800000613
011900000613
012000000613     C*--------------------------------------------------------
012100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012200000613     C*--------------------------------------------------------
012300000621     C     STPR          BEGSR
012400000613     C*
012500050802     C                   if        not %open(titvn5p)
012600050802     C                   open      titvn5p
012700990915     C                   endif
012800990915     C*
012900990915     C                   except    riepilogo
013000990915     C*
013100050802     C                   if        %open(titvn5p)
013200050802     C                   close     titvn5p
013300990914     C                   endif
013400990910     C*
013500000613     C                   ENDSR
013600000613     C***
013700990908
013800000801
013900910830     C*--------------------------------------------------------
014000050802     C* RWVAB     LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR *
014100910830     C*--------------------------------------------------------
014200050802     C     RWVAB         BEGSR
014300990910     C*
014400990914     C                   if        not %open(tivin00r)
014500990908     C                   open      tivin00r
014600990914     C                   endif
014700050802     C                   if        not %open(edivabwr)
014800050802     C                   open      edivabwr
014900990914     C                   endif
015000050802     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015100020305     C                   exsr      prevat
015200010201     C*
015300040428     C                   if        chkcall = '0'
015400010202     C*
015500050802     C                   if        not %open(edivatwr)
015600050802     C                   open      edivatwr
015700010201     C                   endif
015800990910     C*
015900010201     C                   clear                   §CTROKVB          5 0
016000020305     C                   clear                   §CTROKVT          5 0
016100000801     C                   clear                   §CTRMO            5 0
016200000801     C                   clear                   §CTRNO            5 0
016300990910     C*
016400921023     C                   DO        *HIVAL
016500990913     C*
016600990915     C                   READ      tivin00r                               70
016700000905     C                   if        vindta > *blanks
016800000613     C                   add       1             rrnum
016900000801     C*
017000000801     C                   if        *in70 = *off
017100000801     C                             and
017200000801     C                             (vinflg = *blanks
017300000801     C                              or vinflg = '0'
017400000801     C                              or vinflg = '2')
017500000801     C*
017600000801     C                   clear                   vinmsg
017700010305     C*
017800050802     C                   exsr      impvab                                       => carico VAB
017900050802     C                   exsr      wrivab                                       => scrivo VAB
018000010305     C                   endif
018100000905     C*
018200000905     C                   else
018300000905     C                   eval      vinflg = '1'
018400000905     C                   endif
018500000905     C*
018600000905     C  N70              update    tivin000
018700000905     C*
018800991022     C  N70              ENDdo
018900010202     C*
019000010202     C                   endif
019100050802     C*
019200990910     C* Se non ci sono record con errori ...
019300000710     C                   if        §ctrno = 0
019400990910     C* ... restituisco esito OK.
019500990921     C                   eval      wrkesito = '0'
019600990910     C                   else
019700010201     C                   if        §ctrokvb > 0
019800990921     C                   eval      wrkesito = '1'
019900000710     C                   else
020000000710     C                   eval      wrkesito = '2'
020100990910     C                   endif
020200000710     C                   endif
020300990910     C*
020400990914     C                   if        %open(tivin00r)
020500990908     C                   close     tivin00r
020600990914     C                   endif
020700050802     C                   if        %open(edivabwr)
020800050802     C                   close     edivabwr
020900990914     C                   endif
021000050802     C                   if        %open(edivatwr)
021100050802     C                   close     edivatwr
021200010201     C                   endif
021300021017     C*
021400021017     C                   if        vlrpoi <> 999
021500021017     C                   eval      vlrpoi = vlrpoi
021600021017     C                   endif
021700990910     C*
021800010201     C                   if        §ctrokvb > 0
021900021017     C                             and vlrpoi > *zeros
022000010202     C                   exsr      invio
022100990920     C                   endif
022200990920     C*
022300910830     C                   ENDSR
022400000613     C***
022500010305
022600010305     C*----------------------------------------------------*
022700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
022800010305     C*----------------------------------------------------*
022900020305     C     WRIVAB        BEGSR
023000010305     C*
023100050802     C                   write     edivab00                                     => scarico il VAB
023200010305     C*
023300010305     C                   ENDSR
023400990920
023500000801     C*----------------------------------------------------*
023600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700000801     C*----------------------------------------------------*
023800010201     C     INZVAR        BEGSR
023900000801     C*
024000010201     C                   Z-ADD     *zeros        Num5_0
024100030107     C                   Z-ADD     *zeros        Num7_0
024200030711     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024300030711     C                   MOVEL     '0'           FlgCAS            1
024400000801     C*
024500000801     C                   ENDSR
024600000801     C*----------------------------------------------------*
024700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024800000801     C*----------------------------------------------------*
024900000801     C     DEFCAM        BEGSR
025000040428     C*
025100050802     C                   CLEAR                   EDIVAB00
025200050802     C                   CLEAR                   EDIVAT00
025300000801     C*
025400020619     C* Imposto i valori di default...
025500050802     C                   Z-ADD     0051381       VABCCM
025600050802     C                   Z-ADD     0051381       VATCCM
025700021017     C                   Z-ADD     vlrpoi        VABFGS
025800021017     C                   Z-ADD     vlrpoi        VATFGS
025900040428     C                   Z-ADD     005           VABLNP
026000040428     C                   Z-ADD     005           VATLNP
026100050802     C                   Z-ADD     000           VABCTR
026200021017     C                   MOVEL     '7Q'          VABCTM
026300050802     C                   MOVEL     '1'           VABCBO
026400020305     C                   MOVEL     'E'           VATTRC
026500020619     C* ... e poi verifico se sono stati passati come parametri
026600020619     C                   IF        vlrppt > *blanks
026700040428     C                   IF        %subst(vlrppt:1:7) <> *blanks
026800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026900020619     C                   EXSR      CHKNUM
027000020619     C                   IF        PiInt=*on
027100020619     C                   Z-ADD     PiVal         VABCCM
027200020619     C                   Z-ADD     PiVal         VATCCM
027300020619     C                   ENDIF
027400040428     C                   ENDIF
027500040428     C                   IF        %subst(vlrppt:8:3) <> *blanks
027600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027700020619     C                   EXSR      CHKNUM
027800020619     C                   IF        PiInt=*on
027900020619     C                   Z-ADD     PiVal         VABLNP
028000020619     C                   Z-ADD     PiVal         VATLNP
028100020619     C                   ENDIF
028200040428     C                   ENDIF
028300040428     C                   IF        %subst(vlrppt:11:3) <> *blanks
028400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028500020619     C                   EXSR      CHKNUM
028600020619     C                   IF        PiInt=*on
028700020619     C                   Z-ADD     PiVal         VABCTR
028800020619     C                   ENDIF
028900040428     C                   ENDIF
029000020619     C                   ENDIF
029100050802     C*
029200050802     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
029300050802     C                   EVAL      VABCMR = 'TECH DATA'
029400050802     C                   EVAL      VABDCM = datcor
029500050802     C                   EVAL      VABDTS = datcor
029600050802     C                   EVAL      VABHMS = ORACOR
029700050802     C                   EVAL      VABCNT = 1
029800050802     C*
029900050802     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
030000050802     C                   EVAL      VATCMR = 'TECH DATA'
030100050802     C                   EVAL      VATCNT = 1
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500050802     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
030600000801     C*----------------------------------------------------*
030700010201     C     IMPVAB        BEGSR
030800000801     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200000801     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400020305     C                   MOVEL     datcor        VATAAS
031500000830     C                   MOVE      datcor        VABMGS
031600031117     C*
031700050802     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
031800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032000020117     C* ==
032100050802     C                   EVAL      VABRD2=%trim(%subst(vindta:88:35))
032200050802     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
032300050802     C                   EVAL      VABLOD=%trim(%subst(vindta:167:25))
032400050802     C                   EVAL      VABPRD=%trim(%subst(vindta:202:2))
032500050802     C                   EVAL      VABRMA=%trim(%subst(vindta:324:15))
032600050802     C                   EVAL      VABNOT=%trim(%subst(vindta:380:35))
032700050802     C                   EVAL      VABNT2=%trim(%subst(vindta:415:35))
032800050802     C                   EVAL      VABNAS=%trim(%subst(vindta:234:35))
032900051117     C                   IF        %trim(%subst(vindta:47:2)) = '2'
033000051117     C                   EVAL      VABCBO='2'
033100051117     C                   ENDIF
033200030711     C* CAD
033300050802     C                   EVAL      PiStr=%trim(%subst(vindta:158:9))
033400010201     C                   EXSR      CHKNUM
033500010201     C                   IF        PiInt=*on
033600010201     C                   Z-ADD     PiVal         Num5_0
033700021017     C                   MOVEL     Num5_0        VABCAD
033800010201     C                   ELSE
033900010201     C                   ADD       1             errore
034000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034100021017     C                             + ' ' + 'VABCAD'
034200010201     C                   ENDIF
034300030711     C* RMN
034400050802     C                   EVAL      PiStr=%trim(%subst(vindta:308:15))
034500010201     C                   EXSR      CHKNUM
034600010201     C                   IF        PiInt=*on
034700021017     C                   Z-ADD     PiVal         VABRMN
034800010201     C                   ELSE
034900010201     C                   ADD       1             errore
035000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035100050802     C                             + ' ' + 'VABRMN'
035200010201     C                   ENDIF
035300050802     C* NSP
035400050802     C                   EVAL      PiStr=%trim(%subst(vindta:37:7))
035500050802     C                   EXSR      CHKNUM
035600050802     C                   IF        PiInt=*on
035700050802     C                   Z-ADD     PiVal         Num7_0
035800050802     C                   MOVE(P)   Num7_0        VABNSP
035900050802     C                   MOVE(P)   Num7_0        VATNSP
036000050802     C                   ELSE
036100050802     C                   ADD       1             errore
036200050802     C                   EVAL      vinmsg = %trimr(vinmsg)
036300050802     C                             + ' ' + 'VABNSP VATNSP'
036400050802     C                   ENDIF
036500050802     C* NCL
036600050802     C                   EVAL      PiStr=%trim(%subst(vindta:249:6))
036700030107     C                   EXSR      CHKNUM
036800030107     C                   IF        PiInt=*on
036900040428     C                   ADD       PiVal         VABNCL
037000030107     C                   ELSE
037100030107     C                   ADD       1             errore
037200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
037300030107     C                             + ' ' + 'VABNCL'
037400030107     C                   ENDIF
037500050802     C* PKB
037600050802     C                   EVAL      PiStr=%trim(%subst(vindta:255:9))
037700030107     C                   EXSR      CHKNUM
037800030107     C                   IF        PiNum=*on
037900050802     C                   Z-ADD     PiVal         VABPKB
038000030107     C                   ELSE
038100030107     C                   ADD       1             errore
038200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
038300030107     C                             + ' ' + 'VABPKB'
038400030107     C                   ENDIF
038500050802     C* IAS
038600050802     C                   IF        %subst(vindta:216:15) <> *blanks
038700050802     C                   EVAL      PiStr=%trim(%subst(vindta:216:15))
038800050802     C                   EXSR      CHKNUM
038900050802     C                   IF        PiNum=*on
039000050802     C                   Z-ADD     PiVal         VABIAS
039100050802     C                   IF        VABIAS > *zeros
039200050802     C                   EVAL      VABVAS = 'EUR'
039300050802     C                   ENDIF
039400050802     C                   ELSE
039500050802     C                   ADD       1             errore
039600050802     C                   EVAL      vinmsg = %trimr(vinmsg)
039700050802     C                             + ' ' + 'VABIAS'
039800050802     C                   ENDIF
039900050802     C                   ENDIF
040000050802     C* "CHI SONO" - DA
040100050802     C                   EVAL      PiStr=%trim(%subst(vindta:344:15))
040200050802     C                   EXSR      CHKNUM
040300050802     C                   IF        PiInt=*on
040400050802     C                   Z-ADD     PiVal         wWHOAREda        15 0
040500050802     C                   ELSE
040600050802     C                   ADD       1             errore
040700050802     C                   EVAL      vinmsg = %trimr(vinmsg)
040800050802     C                             + ' ' + '"CHI SONO"'
040900050802     C                   ENDIF
041000050802     C* "CHI SONO" - A
041100050802     C                   EVAL      PiStr=%trim(%subst(vindta:364:15))
041200050802     C                   EXSR      CHKNUM
041300050802     C                   IF        PiInt=*on
041400050802     C                   Z-ADD     PiVal         wWHOAREa         15 0
041500050802     C                   ELSE
041600050802     C                   ADD       1             errore
041700050802     C                   EVAL      vinmsg = %trimr(vinmsg)
041800050802     C                             + ' ' + '"CHI SONO"'
041900050802     C                   ENDIF
042000050802     C* Controllo range "CHI SONO" / VABNCL
042100050802     C                   IF        wWHOAREa - wWHOAREda + 1 <> VABNCL
042200050802     C                   ADD       1             errore
042300050802     C                   EVAL      vinmsg = %trimr(vinmsg)
042400090723     C                             + ' ' + 'VABNCL + "CHI SONO"'
042500050802     C                   ENDIF
042600010205     C*
042700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042800051117     C                   IF        FlgCAS <> '0'
042900051117     C                   IF        VABCBO = '1'
043000040428     C                   EVAL      VABCBO = '4'                                 * franco  SI contr.
043100051117     C                   ENDIF
043200051117     C                   IF        VABCBO = '2'
043300051117     C                   EVAL      VABCBO = '6'                                 * franco  SI contr.
043400051117     C                   ENDIF
043500040428     C                   EVAL      VABVCA = 'EUR'
043600031117     C                   ENDIF
043700020305     C*
043800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043900011113     C                   EXSR      CHKIMPDIV
044000050802     C*
044100050802     C* Eseguo routine x esplosione "CHI SONO"
044200050802     C                   EXSR      wrivat
044300010202     C*
044400000801     C* Ebbene...
044500000801     C                   ADD       1             §CTRMO
044600010201     C                   IF        errore <> *zeros
044700000801     C                   ADD       1             §CTRNO
044800000801     C                   EVAL      recko = vindta
044900000801     C                   EXCEPT    rigadett
045000000801     C                   EVAL      vinflg = '2'
045100000801     C                   ELSE
045200010201     C                   ADD       1             §CTROKVB
045300000801     C                   ENDIF
045400000801     C*
045500000801     C                   ENDSR
045600010201     C*----------------------------------------------------*
045700050802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
045800010201     C*----------------------------------------------------*
045900020305     C     WRIVAT        BEGSR
046000040428     C*
046100040428     C* X ogni collo indicato in testata
046200050802     C                   z-add     wWHOAREda     wWHOARE          15 0
046300050802     C                   dow       wWHOARE <= wWHOAREa
046400010201     C*
046500050802     C* Valorizzo il buffer di scrittura del EDIVAT
046600050802     C                   movel(p)  wWHOARE       VATNOT
046700050802     C                   write     EDIVAT00
046800040428     C*
046900050802     C                   add       1             wWHOARE
047000050802     C                   add       1             §ctrokvt
047100040428     C                   enddo
047200010201     C*
047300010201     C                   ENDSR
047400010202     C*----------------------------------------------------*
047500050802     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047600010202     C*----------------------------------------------------*
047700020305     C     PREVAT        BEGSR
047800010202     C*
047900050802     C* Compongo il nome del membro da dare al EDIVATWR
048000010202     C                   eval      parmbr = vlrhdl
048100010202     C                   movel     'M'           parmbr
048200050802     C                   eval      parccm = '00051381'
048300010202     C                   eval      paropz = '1'
048400010202     C* Effettuo la chiamata al CLLE preposto
048500050802     C                   call(e)   'TITVEVTC'
048600010202     C                   parm                    parccm
048700010202     C                   parm                    parmbr
048800010202     C                   parm                    paropz
048900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049000010202     C                   if        %error
049100010202     C                   movel     '1'           chkcall
049200010202     C                   else
049300010202     C                   movel     '0'           chkcall
049400010202     C                   endif
049500010202     C*
049600010202     C                   ENDSR
049700000801     C*----------------------------------------------------*
049800000801     C*  CONTROLLO NUMERICITA' CAMPI
049900000801     C*----------------------------------------------------*
050000000801     C     CHKNUM        BEGSR
050100000801     C*
050200000801     C                   call(e)   'ISNUMERIC'
050300000801     C                   PARM                    PiStr            30
050400030711     C                   PARM      '.'           PiDecChr          1
050500000801     C                   PARM      *ZEROS        PiVal            30 9
050600000801     C                   PARM      '0'           PiInt             1
050700000801     C                   PARM      '0'           PiNum             1
050800000801     C                   IF        %error
050900000801     C                   EVAL      PiInt=*off
051000030107     C                   EVAL      PiNum=*off
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
057000021017
057100000801
057200000801
057300990920      /TITLE Invio dei dati al punto operativo.
057400010202     C     invio         BEGSR
057500990920     C*
057600050802     C* 1° invio EDIVAT
057700010201     C                   reset                   dscmz
057800021017     C                   move      vlrpoi        cmzdst
057900050802     C                   eval      cmzfld = 'EDIVATWR'
058000010201     C                   eval      cmzmbd = vlrhdl
058100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058200021009     C***                if        prmfir = *blanks
058300050802     C                   eval      cmzfla = 'EDIVAT0F'
058400050802     C                   eval      cmzmba = 'EDIVAT0F'
058500021009     C***                else
058600021009     C***                eval      cmzfla = prmfir
058700021009     C***                eval      cmzmba = prmfir
058800021009     C***                endif
058900010201     C                   eval      cmznrr = *zeros
059000020305     C                   move      §ctrokvt      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010205     C                   if        %error
059600010205     C                             or cmzerr = '1'
059700010205     C                             or esito  = '1'
059800010205     C                   eval      wrkesito = '3'
059900010205     C                   else
060000010201     C*
060100050802     C* 2° invio EDIVAB
060200010201     C                   reset                   dscmz
060300021017     C                   move      vlrpoi        cmzdst
060400010201     C                   eval      cmzfld = vlrfou
060500010201     C                   eval      cmzmbd = vlrhdl
060600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060700021009     C***                if        prmfir = *blanks
060800050802     C                   eval      cmzfla = 'EDIVAB0F'
060900050802     C                   eval      cmzmba = 'EDIVAB0F'
061000021009     C***                else
061100021009     C***                eval      cmzfla = prmfir
061200021009     C***                eval      cmzmba = prmfir
061300021009     C***                endif
061400010201     C                   eval      cmznrr = *zeros
061500010201     C                   move      §ctrokvb      cmznrr
061600021018     C                   eval      cmzlba = vlrfl1
061700010201     C                   call(e)   'TIS711C'
061800010201     C                   parm                    dscmz
061900010201     C                   parm      *blanks       esito
062000010201     C                   if        %error
062100010201     C                             or cmzerr = '1'
062200010201     C                             or esito  = '1'
062300010201     C                   eval      wrkesito = '3'
062400010201     C                   endif
062500010205     C                   endif
062600990920     C*
062700000613     C                   ENDSR
062800000613     C***
062900070502
063000070502
063100070502
063200070502     C     *pssr         BEGSR
063300070502     C*
063400070502     C                   if        %open(edivabwr)
063500070502     C                   close     edivabwr
063600070502     C                   endif
063700070502     C                   if        %open(edivatwr)
063800070502     C                   close     edivatwr
063900070502     C                   endif
064000070502     C*
064100070502     C* Effettuo la chiamata al CLLE preposto
064200070502     C                   call(e)   'TITVEVTC'
064300070502     C                   parm                    parccm
064400070502     C                   parm                    parmbr
064500070502     C                   parm      '2'           paropz
064600070502     C*
064700070502     C                   eval      wrkesito = '2'
064800070502     C*
064900070502     C                   seton                                        LR
065000070502     C*
065100070502     C                   ENDSR     '*CANCL'
065200070502     C***
065300070502
065400070502
065500990910
065600000613     C     *inzsr        BEGSR
065700990910     C*
065800990910     C     *entry        plist
065900990920     C                   parm                    tivlrds
066000990921     C                   parm      wrkesito      esito
066100000724     C                   parm                    prmlit
066200000710     C                   parm                    prmfir
066300000613     C*
066400000830     C* CALCOLA LA DATA CORRENTE
066500000830     C                   time                    wn14             14 0
066600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066700050802     C                   movel     wn14          oracor            6 0          *ORA
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
067800050802     Otitvn5p   E            riepilogo         2
067900990915     O                                              'Upload via Internet'
068000990915     O                                           +1 'Traduzione TIVIN00R -'
068100050802     O                                           +1 'EDIVABWR/EDIVATWR'
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
070300050802     Otitvn5ps  E            testdett          2
070400000613     O                                              'Upload via Internet'
070500000613     O                                           +1 'Traduzione TIVIN00R -'
070600050802     O                                           +1 'EDIVABWR/EDIVATWR'
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
073400050802OVRPRTF FILE(TITVN5P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
073500050802OVRPRTF FILE(TITVN5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
073600050802DLTOVR FILE(TITVN5P TITVN5PS) LVL(*)
073700000512
073800000512
