000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800030730     Ftitv62p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030730     Ftitv62ps  O    f  198        PRINTER usropn
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
003500030415     D depspe          s              7    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000040407     D SavRifCli       s              7  0 DIM(5000) INZ(*zeros)
004100040407     D wRifCli         s              7  0 INZ(9990000)
004200040407     D wContaRifCli    s              4  0 INZ(*zeros)
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
009400030730     C                   if        not %open(titv62ps)
009500030730     C                   open      titv62ps
009600000616     C                   except    testdett
009700000613     C                   endif
009800000613     C*
009900030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010100000613     C*
010200010202     C* Effettuo la chiamata al CLLE preposto
010300030730     C                   call(e)   'TITVVTC'
010400010202     C                   parm                    parccm
010500010202     C                   parm                    parmbr
010600010202     C                   parm      '2'           paropz
010700010202     C*
010800030730     C                   if        %open(titv62ps)
010900000616     C                   except    findett
011000030730     C                   close     titv62ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900010201     C                   seton                                        LR
012000000613
012100000613
012200000613     C*--------------------------------------------------------
012300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012400000613     C*--------------------------------------------------------
012500000621     C     STPR          BEGSR
012600000613     C*
012700030730     C                   if        not %open(titv62p)
012800030730     C                   open      titv62p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300030730     C                   if        %open(titv62p)
013400030730     C                   close     titv62p
013500990914     C                   endif
013600990910     C*
013700000613     C                   ENDSR
013800000613     C***
013900990908
014000000801
014100910830     C*--------------------------------------------------------
014200030730     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014300910830     C*--------------------------------------------------------
014400030226     C     RWFILE        BEGSR
014500990910     C*
014600990914     C                   if        not %open(tivin00r)
014700990908     C                   open      tivin00r
014800990914     C                   endif
014900021017     C                   if        not %open(fivabwwr)
015000021017     C                   open      fivabwwr
015100990914     C                   endif
015200021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300020305     C                   exsr      prevat
015400010201     C*
015500010202     C                   if        chkcall = '0'
015600010202     C*
015700021017     C                   if        not %open(fivatwwr)
015800021017     C                   open      fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   clear                   §CTROKVB          5 0
016200020305     C                   clear                   §CTROKVT          5 0
016300000801     C                   clear                   §CTRMO            5 0
016400000801     C                   clear                   §CTRNO            5 0
016500990910     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900000905     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000801     C*
017200000801     C                   if        *in70 = *off
017300000801     C                             and
017400000801     C                             (vinflg = *blanks
017500000801     C                              or vinflg = '0'
017600000801     C                              or vinflg = '2')
017700000801     C*
017800000801     C                   clear                   vinmsg
017900030310     C                   eval      vinflg = '1'
018000030310     C*
018100030506     C* Determino se il record corrente è di tipo testata o di tipo dettaglio
018200030730     C                   if        %subst(vindta:1:1) = 'T'                     *** TESTATA ***
018300030506     C                   exsr      impvab                                       => carico  VAB
018400030506     C                   exsr      wrivab                                       => scarico VAB
018500030506     C                   endif
018600030730     C                   if        %subst(vindta:1:1) = 'D'                     *** DETTAGLIO ***
018700030506     C                   exsr      impvat                                       => carico  VAT
018800030506     C                   exsr      wrivat                                       => scarico VAT
018900030506     C                   endif
019000000905     C*
019100000905     C                   else
019200000905     C                   eval      vinflg = '1'
019300000905     C                   endif
019400030411     C                   endif
019500000905     C*
019600000905     C  N70              update    tivin000
019700000905     C*
019800991022     C  N70              ENDdo
019900010202     C*
020000010202     C                   endif
020100990910
020200990910     C* Se non ci sono record con errori ...
020300000710     C                   if        §ctrno = 0
020400990910     C* ... restituisco esito OK.
020500990921     C                   eval      wrkesito = '0'
020600990910     C                   else
020700010201     C                   if        §ctrokvb > 0
020800990921     C                   eval      wrkesito = '1'
020900000710     C                   else
021000000710     C                   eval      wrkesito = '2'
021100990910     C                   endif
021200000710     C                   endif
021300990910     C*
021400990914     C                   if        %open(tivin00r)
021500990908     C                   close     tivin00r
021600990914     C                   endif
021700021017     C                   if        %open(fivabwwr)
021800021017     C                   close     fivabwwr
021900990914     C                   endif
022000021017     C                   if        %open(fivatwwr)
022100021017     C                   close     fivatwwr
022200010201     C                   endif
022300021017     C*
022400021017     C                   if        vlrpoi <> 999
022500021017     C                   eval      vlrpoi = vlrpoi
022600021017     C                   endif
022700990910     C*
022800010201     C                   if        §ctrokvb > 0
022900021017     C                             and vlrpoi > *zeros
023000010202     C                   exsr      invio
023100990920     C                   endif
023200990920     C*
023300910830     C                   ENDSR
023400000613     C***
023500990920
023600000801     C*----------------------------------------------------*
023700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023800000801     C*----------------------------------------------------*
023900010201     C     INZVAR        BEGSR
024000000801     C*
024100010201     C                   Z-ADD     *zeros        Num5_0
024200030107     C                   Z-ADD     *zeros        Num7_0
024300060620     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024400060620     C                   MOVEL     '0'           FlgCAS            1
024500000801     C*
024600000801     C                   ENDSR
024700000801     C*----------------------------------------------------*
024800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024900000801     C*----------------------------------------------------*
025000000801     C     DEFCAM        BEGSR
025100000801     C*
025200020619     C* Imposto i valori di default...
025300030730     C                   Z-ADD     0240286       VABCCM
025400030730     C                   Z-ADD     0240286       VATCCM
025500021017     C                   Z-ADD     vlrpoi        VABFGS
025600021017     C                   Z-ADD     vlrpoi        VATFGS
025700030730     C                   Z-ADD     024           VABLNP
025800030730     C                   Z-ADD     024           VATLNP
025900030730     C                   Z-ADD     100           VABCTR
026000021017     C                   MOVEL     '7Q'          VABCTM
026100020305     C                   MOVEL     'E'           VATTRC
026200020619     C* ... e poi verifico se sono stati passati come parametri
026300020619     C                   IF        vlrppt > *blanks
026400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026500020619     C                   EXSR      CHKNUM
026600020619     C                   IF        PiInt=*on
026700020619     C                   Z-ADD     PiVal         VABCCM
026800020619     C                   Z-ADD     PiVal         VATCCM
026900020619     C                   ENDIF
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABLNP
027400020619     C                   Z-ADD     PiVal         VATLNP
027500020619     C                   ENDIF
027600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027700020619     C                   EXSR      CHKNUM
027800020619     C                   IF        PiInt=*on
027900020619     C                   Z-ADD     PiVal         VABCTR
028000020619     C                   ENDIF
028100020619     C                   ENDIF
028200000801     C*
028300000801     C                   ENDSR
028400000801     C*----------------------------------------------------*
028500030730     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028600000801     C*----------------------------------------------------*
028700010201     C     IMPVAB        BEGSR
028800060918     C*
028900060918     C* Inizializzo il buffer d output
029000060918     C                   CLEAR                   FIVAB000
029100030730     C*
029200020305     C                   EXSR      INZVAR
029300020305     C                   EXSR      DEFCAM
029400010305     C*
029500030226     C                   Z-ADD     *zeros        errore            1 0
029600000830     C                   MOVEL     datcor        VABAAS
029700000830     C                   MOVE      datcor        VABMGS
029800030226     C*
029900030730     C                   EVAL      VABRSD=%trim(%subst(vindta:21:40))
030000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030200020117     C* ==
030300030730     C                   EVAL      VABIND=%trim(%subst(vindta:61:40))
030400030730     C                   EVAL      VABLOD=%trim(%subst(vindta:106:30))
030500030730     C                   EVAL      VABPRD=%trim(%subst(vindta:136:2))
030600030730     C                   EVAL      VABNOT=%trim(%subst(vindta:150:30))
030700030730     C                   EVAL      VABRMA=%trim(%subst(vindta:6:6))
030800030506     C                   EVAL      VABCBO = '1'
030900030226     C* CAP
031000030730     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
031100010201     C                   EXSR      CHKNUM
031200010201     C                   IF        PiInt=*on
031300010201     C                   Z-ADD     PiVal         Num5_0
031400021017     C                   MOVEL     Num5_0        VABCAD
031500010201     C                   ELSE
031600010201     C                   ADD       1             errore
031700010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031800021017     C                             + ' ' + 'VABCAD'
031900010201     C                   ENDIF
032000040407     C* NSP/RMN
032100030730     C                   EVAL      PiStr=%trim(%subst(vindta:6:6))
032200010201     C                   EXSR      CHKNUM
032300010201     C                   IF        PiInt=*on
032400030107     C                   Z-ADD     PiVal         Num7_0
032500040407     C                   MOVE(P)   Num7_0        VABRMN
032600040407     C                   MOVE(P)   Num7_0        VABNSP
032700040407     C                   MOVE(P)   Num7_0        VATNSP
032800040407     C* Testo che il numero spedizione indicato dal cliente NN sia già stato utilizzato dal cliente
032900040407     C* nell'archivio corrente
033000040407     C     VABNSP        lookup    SavRifCli                              55
033100040407     C                   if        %found
033200040407     C                   eval      wRifCli = wRifCli + 1
033300040407     C                   MOVE(P)   wRifCli       VABNSP
033400040407     C                   MOVE(P)   wRifCli       VATNSP
033500040407     C                   endif
033600040407     C                   add       1             wContaRifCli
033700040407     C                   eval      SavRifCli(wContaRifCli) = VABNSP
033800040407     C*
033900010201     C                   ELSE
034000010201     C                   ADD       1             errore
034100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034200030506     C                             + ' ' + 'VABNSP VABRMN VATNSP'
034300010201     C                   ENDIF
034400030226     C* NCL
034500030730     C                   EVAL      PiStr=%trim(%subst(vindta:138:4))
034600030430     C                   EXSR      CHKNUM
034700030430     C                   IF        PiInt=*on
034800030430     C                   Z-ADD     PiVal         VABNCL
034900030430     C                   ELSE
035000030430     C                   ADD       1             errore
035100030430     C                   EVAL      vinmsg = %trimr(vinmsg)
035200030430     C                             + ' ' + 'VABNCL'
035300030430     C                   ENDIF
035400030506     C* PKB
035500030730     C                   EVAL      PiStr=%trim(%subst(vindta:142:8))
035600030107     C                   EXSR      CHKNUM
035700030107     C                   IF        PiNum=*on
035800030730     C                   EVAL(H)   VABPKB = PiVal/100
035900030107     C                   ELSE
036000030107     C                   ADD       1             errore
036100030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036200030107     C                             + ' ' + 'VABPKB'
036300030107     C                   ENDIF
036400060620     C* CAS
036500060620     C                   IF        %subst(vindta:191:1) = '1'
036600060620     C                   EVAL      FlgCAS='1'
036700060620     C                   EVAL      PiStr=%trim(%subst(vindta:192:11))
036800060620     C                   EXSR      CHKNUM
036900060620     C                   IF        PiNum=*on
037000060620     C                   EVAL(H)   VABCAS = PiVal/100
037100060620     C                   EVAL      VABVCA='EUR'
037200060620     C                   ELSE
037300060620     C                   SETON                                        32
037400060620     C                   Z-ADD     *zeros        VABCAS
037500060620     C                   EVAL      vinmsg = %trimr(vinmsg)
037600060620     C                             + ' ' + 'VABCAS'
037700060620     C                   ENDIF
037800060620     C                   ENDIF
037900060620     C*
038000060620     C* Considerazioni finali su CBO/CAS
038100060620     C                   IF        FlgCAS = '1'
038200060620     C                   IF        VABCBO = '1'
038300060620     C                   EVAL      VABCBO = '4'
038400060620     C                   ENDIF
038500060620     C                   IF        VABCBO = '2'
038600060620     C                   EVAL      VABCBO = '6'
038700060620     C                   ENDIF
038800060620     C                   ENDIF
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
040500030226     C*----------------------------------------------------*
040600030226     C*  SCARICAMENTO BUFFER RECORDS VAB
040700030226     C*----------------------------------------------------*
040800030226     C     WRIVAB        BEGSR
040900030310     C*
041000030310     C                   write     FIVAB000
041100030226     C*
041200030226     C                   ENDSR
041300030226     C*----------------------------------------------------*
041400030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
041500030226     C*----------------------------------------------------*
041600030226     C     IMPVAT        BEGSR
041700030226     C*
041800030226     C                   MOVEL     datcor        VATAAS
041900030926     C*
042000040217     C* A seconda del codice cliente mittente prendo posizini diverse
042100040217     C                   IF        VATCCM = 0240286
042200030926     C                   EVAL      INBARCODE = %trim(%subst(vindta:11:6))
042300040217     C                   ENDIF
042400040217     C                   IF        VATCCM = 0240342
042500040217     C                   EVAL      INBARCODE = %trim(%subst(vindta:2:6))
042600040217     C                   ENDIF
042700040217     C* Dal segnacollo cliente genero il barcode dell'etichetta secondo lo "standard"
042800040217     C* INTERLEAVE 2/5 (tramite TRILITV25R)
042900030926     C                   CLEAR                   OUTBARCODE
043000030929     C                   CALL      'TRULIL25R'
043100030929     C                   PARM                    INBARCODE        10
043200030929     C                   PARM                    OUTBARCODE       30
043300030226     C*
043400030415     C* Valorizzo il barcode
043500030926     C                   EVAL      VATNOT=%trim(OUTBARCODE)
043600030226     C*
043700030226     C                   ENDSR
043800010201     C*----------------------------------------------------*
043900021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
044000010201     C*----------------------------------------------------*
044100020305     C     WRIVAT        BEGSR
044200010201     C*
044300021017     C                   write     FIVAT000
044400010201     C*
044500010201     C                   ENDSR
044600010202     C*----------------------------------------------------*
044700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044800010202     C*----------------------------------------------------*
044900020305     C     PREVAT        BEGSR
045000010202     C*
045100021017     C* Compongo il nome del membro da dare al FIVATWWR
045200010202     C                   eval      parmbr = vlrhdl
045300010202     C                   movel     'M'           parmbr
045400030730     C                   eval      parccm = '00240286'
045500010202     C                   eval      paropz = '1'
045600010202     C* Effettuo la chiamata al CLLE preposto
045700030730     C                   call(e)   'TITVVTC'
045800010202     C                   parm                    parccm
045900010202     C                   parm                    parmbr
046000010202     C                   parm                    paropz
046100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046200010202     C                   if        %error
046300010202     C                   movel     '1'           chkcall
046400010202     C                   else
046500010202     C                   movel     '0'           chkcall
046600010202     C                   endif
046700010202     C*
046800010202     C                   ENDSR
046900000801     C*----------------------------------------------------*
047000000801     C*  CONTROLLO NUMERICITA' CAMPI
047100000801     C*----------------------------------------------------*
047200000801     C     CHKNUM        BEGSR
047300000801     C*
047400000801     C                   call(e)   'ISNUMERIC'
047500000801     C                   PARM                    PiStr            30
047600030430     C                   PARM      '.'           PiDecChr          1
047700000801     C                   PARM      *ZEROS        PiVal            30 9
047800000801     C                   PARM      '0'           PiInt             1
047900000801     C                   PARM      '0'           PiNum             1
048000000801     C                   IF        %error
048100000801     C                   EVAL      PiInt=*off
048200030107     C                   EVAL      PiNum=*off
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
054200021017
054300000801
054400000801
054500990920      /TITLE Invio dei dati al punto operativo.
054600010202     C     invio         BEGSR
054700990920     C*
054800021017     C* 1° invio FIVAT
054900010201     C                   reset                   dscmz
055000021017     C                   move      vlrpoi        cmzdst
055100021017     C                   eval      cmzfld = 'FIVATWWR'
055200010201     C                   eval      cmzmbd = vlrhdl
055300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055400021009     C***                if        prmfir = *blanks
055500021017     C                   eval      cmzfla = 'FIVAT00F'
055600021017     C                   eval      cmzmba = 'FIVAT00F'
055700021009     C***                else
055800021009     C***                eval      cmzfla = prmfir
055900021009     C***                eval      cmzmba = prmfir
056000021009     C***                endif
056100010201     C                   eval      cmznrr = *zeros
056200020305     C                   move      §ctrokvt      cmznrr
056300021018     C                   eval      cmzlba = vlrfl1
056400010201     C                   call(e)   'TIS711C'
056500010201     C                   parm                    dscmz
056600010201     C                   parm      *blanks       esito
056700010205     C                   if        %error
056800010205     C                             or cmzerr = '1'
056900010205     C                             or esito  = '1'
057000010205     C                   eval      wrkesito = '3'
057100010205     C                   else
057200010201     C*
057300021017     C* 2° invio FIVAB
057400010201     C                   reset                   dscmz
057500021017     C                   move      vlrpoi        cmzdst
057600010201     C                   eval      cmzfld = vlrfou
057700010201     C                   eval      cmzmbd = vlrhdl
057800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057900021009     C***                if        prmfir = *blanks
058000021017     C                   eval      cmzfla = 'FIVAB00F'
058100021017     C                   eval      cmzmba = 'FIVAB00F'
058200021009     C***                else
058300021009     C***                eval      cmzfla = prmfir
058400021009     C***                eval      cmzmba = prmfir
058500021009     C***                endif
058600010201     C                   eval      cmznrr = *zeros
058700010201     C                   move      §ctrokvb      cmznrr
058800021018     C                   eval      cmzlba = vlrfl1
058900010201     C                   call(e)   'TIS711C'
059000010201     C                   parm                    dscmz
059100010201     C                   parm      *blanks       esito
059200010201     C                   if        %error
059300010201     C                             or cmzerr = '1'
059400010201     C                             or esito  = '1'
059500010201     C                   eval      wrkesito = '3'
059600010201     C                   endif
059700010205     C                   endif
059800990920     C*
059900000613     C                   ENDSR
060000000613     C***
060100070411
060200070411     C     *pssr         BEGSR
060300070411     C*
060400070411     C                   if        %open(tivin00r)
060500070411     C                   close     tivin00r
060600070411     C                   endif
060700070411     C                   if        %open(fivabwwr)
060800070411     C                   close     fivabwwr
060900070411     C                   endif
061000070411     C                   if        %open(fivatwwr)
061100070411     C                   close     fivatwwr
061200070411     C                   endif
061300070411     C*
061400070411     C* Effettuo la chiamata al CLLE preposto
061500070411     C                   call(e)   'TITVVTC'
061600070411     C                   parm                    parccm
061700070411     C                   parm                    parmbr
061800070411     C                   parm      '2'           paropz
061900070411     C*
062000070411     C                   eval      wrkesito = '2'
062100070411     C*
062200070411     C                   seton                                        LR
062300070411     C*
062400070411     C                   ENDSR     '*CANCL'
062500070411     C***
062600070411
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
064900030730     Otitv62p   E            riepilogo         2
065000990915     O                                              'Upload via Internet'
065100990915     O                                           +1 'Traduzione TIVIN00R -'
065200021017     O                                           +1 'FIVABWWR/FIVATWWR'
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
067400030730     Otitv62ps  E            testdett          2
067500000613     O                                              'Upload via Internet'
067600000613     O                                           +1 'Traduzione TIVIN00R -'
067700021017     O                                           +1 'FIVABWWR/FIVATWWR'
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
070500030730OVRPRTF FILE(TITV62P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070600030730OVRPRTF FILE(TITV62PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
070700030730DLTOVR FILE(TITV62P TITV62PS) LVL(*)
070800000512
070900000512
