000100101105      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600101105     FEDIVABwr  O    E             DISK    usropn
000700101105     FEDIVATwr  O    E             DISK    usropn
000800030415     Ftitv50p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100030415     Ftitv50ps  O    f  198        PRINTER usropn
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
003900010213
004000000801     D  Num5_0         s              5  0
004100030107     D  Num7_0         s              7  0
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
006200021017     C                   if        vlrpoi <> *zeros
006300000724     C*
006400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600021017     C     vlrpoi        chain     azorg01l
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
009000030415     C                   if        not %open(titv50ps)
009100030415     C                   open      titv50ps
009200000616     C                   except    testdett
009300000613     C                   endif
009400000613     C*
009500030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009700000613     C*
009800010202     C* Effettuo la chiamata al CLLE preposto
009900101105     C                   call(e)   'TITVEVTC'
010000010202     C                   parm                    parccm
010100010202     C                   parm                    parmbr
010200010202     C                   parm      '2'           paropz
010300010202     C*
010400030415     C                   if        %open(titv50ps)
010500000616     C                   except    findett
010600030415     C                   close     titv50ps
010700000613     C                   endif
010800000616     C*
010900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    CMD(3)
011300000616     C                   PARM                    LENGH
011400000616     C*
011500010201     C                   seton                                        LR
011600000613
011700000613
011800000613     C*--------------------------------------------------------
011900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012000000613     C*--------------------------------------------------------
012100000621     C     STPR          BEGSR
012200000613     C*
012300030415     C                   if        not %open(titv50p)
012400030415     C                   open      titv50p
012500990915     C                   endif
012600990915     C*
012700990915     C                   except    riepilogo
012800990915     C*
012900030415     C                   if        %open(titv50p)
013000030415     C                   close     titv50p
013100990914     C                   endif
013200990910     C*
013300000613     C                   ENDSR
013400000613     C***
013500990908
013600000801
013700910830     C*--------------------------------------------------------
013800101105     C* RWFILE LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR    *
013900910830     C*--------------------------------------------------------
014000030226     C     RWFILE        BEGSR
014100990910     C*
014200990914     C                   if        not %open(tivin00r)
014300990908     C                   open      tivin00r
014400990914     C                   endif
014500101105     C                   if        not %open(edivabwr)
014600101105     C                   open      edivabwr
014700990914     C                   endif
014800101105     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014900020305     C                   exsr      prevat
015000010201     C*
015100010202     C                   if        chkcall = '0'
015200010202     C*
015300101105     C                   if        not %open(edivatwr)
015400101105     C                   open      edivatwr
015500010201     C                   endif
015600990910     C*
015700010201     C                   clear                   §CTROKVB          5 0
015800020305     C                   clear                   §CTROKVT          5 0
015900000801     C                   clear                   §CTRMO            5 0
016000000801     C                   clear                   §CTRNO            5 0
016100990910     C*
016200921023     C                   DO        *HIVAL
016300990913     C*
016400990915     C                   READ      tivin00r                               70
016500000905     C                   if        vindta > *blanks
016600000613     C                   add       1             rrnum
016700000801     C*
016800000801     C                   if        *in70 = *off
016900000801     C                             and
017000000801     C                             (vinflg = *blanks
017100000801     C                              or vinflg = '0'
017200000801     C                              or vinflg = '2')
017300000801     C*
017400000801     C                   clear                   vinmsg
017500030310     C                   eval      vinflg = '1'
017600030411     C*
017700101105     C                   clear                   edivab00
017800101105     C                   clear                   edivat00
017900030411     C                   exsr      impvab                                       => carico VAB
018000030411     C                   exsr      impvat                                       => carico VAT
018100030411     C                   exsr      wrivab                                       => scarico VAB
018200030411     C                   exsr      wrivat                                       => scarico VAT
018300000905     C*
018400000905     C                   else
018500000905     C                   eval      vinflg = '1'
018600000905     C                   endif
018700030411     C                   endif
018800000905     C*
018900000905     C  N70              update    tivin000
019000000905     C*
019100991022     C  N70              ENDdo
019200010202     C*
019300010202     C                   endif
019400990910
019500990910     C* Se non ci sono record con errori ...
019600000710     C                   if        §ctrno = 0
019700990910     C* ... restituisco esito OK.
019800990921     C                   eval      wrkesito = '0'
019900990910     C                   else
020000010201     C                   if        §ctrokvb > 0
020100990921     C                   eval      wrkesito = '1'
020200000710     C                   else
020300000710     C                   eval      wrkesito = '2'
020400990910     C                   endif
020500000710     C                   endif
020600990910     C*
020700990914     C                   if        %open(tivin00r)
020800990908     C                   close     tivin00r
020900990914     C                   endif
021000101105     C                   if        %open(edivabwr)
021100101105     C                   close     edivabwr
021200990914     C                   endif
021300101105     C                   if        %open(edivatwr)
021400101105     C                   close     edivatwr
021500010201     C                   endif
021600021017     C*
021700021017     C                   if        vlrpoi <> 999
021800021017     C                   eval      vlrpoi = vlrpoi
021900021017     C                   endif
022000990910     C*
022100010201     C                   if        §ctrokvb > 0
022200021017     C                             and vlrpoi > *zeros
022300010202     C                   exsr      invio
022400990920     C                   endif
022500990920     C*
022600910830     C                   ENDSR
022700000613     C***
022800990920
022900000801     C*----------------------------------------------------*
023000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023100000801     C*----------------------------------------------------*
023200010201     C     INZVAR        BEGSR
023300000801     C*
023400010201     C                   Z-ADD     *zeros        Num5_0
023500030107     C                   Z-ADD     *zeros        Num7_0
023600030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023700030310     C                   MOVEL     '0'           FlgCAS            1
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
024200000801     C*----------------------------------------------------*
024300000801     C     DEFCAM        BEGSR
024400000801     C*
024500020619     C* Imposto i valori di default...
024600030415     C                   Z-ADD     0492593       VABCCM
024700030415     C                   Z-ADD     0492593       VATCCM
024800021017     C                   Z-ADD     vlrpoi        VABFGS
024900021017     C                   Z-ADD     vlrpoi        VATFGS
025000030415     C                   Z-ADD     049           VABLNP
025100030415     C                   Z-ADD     049           VATLNP
025200101210     C                   Z-ADD     000           VABCTR
025300021017     C                   MOVEL     '7Q'          VABCTM
025400020305     C                   MOVEL     'E'           VATTRC
025500020619     C* ... e poi verifico se sono stati passati come parametri
025600020619     C                   IF        vlrppt > *blanks
025700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025800020619     C                   EXSR      CHKNUM
025900020619     C                   IF        PiInt=*on
026000020619     C                   Z-ADD     PiVal         VABCCM
026100020619     C                   Z-ADD     PiVal         VATCCM
026200020619     C                   ENDIF
026300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026400020619     C                   EXSR      CHKNUM
026500020619     C                   IF        PiInt=*on
026600020619     C                   Z-ADD     PiVal         VABLNP
026700020619     C                   Z-ADD     PiVal         VATLNP
026800020619     C                   ENDIF
026900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000020619     C                   EXSR      CHKNUM
027100020619     C                   IF        PiInt=*on
027200020619     C                   Z-ADD     PiVal         VABCTR
027300020619     C                   ENDIF
027400020619     C                   ENDIF
027500000801     C*
027600000801     C                   ENDSR
027700000801     C*----------------------------------------------------*
027800030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027900000801     C*----------------------------------------------------*
028000010201     C     IMPVAB        BEGSR
028100000801     C*
028200020305     C                   EXSR      INZVAR
028300020305     C                   EXSR      DEFCAM
028400010305     C*
028500030226     C                   Z-ADD     *zeros        errore            1 0
028600000830     C                   MOVEL     datcor        VABAAS
028700000830     C                   MOVE      datcor        VABMGS
028800030226     C*
028900030415     C                   EVAL      VABRSD=%trim(%subst(vindta:32:30))
029000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029200020117     C* ==
029300030415     C                   IF        %subst(vindta:62:3) = 'IT'
029400030415     C                   ELSE
029500030415     C                   EVAL      VABNZD=%trim(%subst(vindta:62:3))
029600030415     C                   ENDIF
029700030415     C                   EVAL      VABIND=%trim(%subst(vindta:65:30))
029800030415     C                   EVAL      VABLOD=%trim(%subst(vindta:155:30))
029900030415     C                   EVAL      VABPRD=%trim(%subst(vindta:95:30))
030000030415     C                   EVAL      VABRMA=%trim(%subst(vindta:22:10))
030100030311     C                   EVAL      VABVCA='EUR'
030200030226     C* CAP
030300030415     C                   EVAL      PiStr=%trim(%subst(vindta:185:10))
030400010201     C                   EXSR      CHKNUM
030500010201     C                   IF        PiInt=*on
030600010201     C                   Z-ADD     PiVal         Num5_0
030700021017     C                   MOVEL     Num5_0        VABCAD
030800010201     C                   ELSE
030900010201     C                   ADD       1             errore
031000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
031100021017     C                             + ' ' + 'VABCAD'
031200010201     C                   ENDIF
031300030311     C* NSP
031400060104     C***                EVAL      PiStr=%trim(%subst(vindta:7:7))
031500060104     C                   EVAL      PiStr=%trim(%subst(vindta:14+1:8-1))
031600010201     C                   EXSR      CHKNUM
031700010201     C                   IF        PiInt=*on
031800030107     C                   Z-ADD     PiVal         Num7_0
031900030107     C                   MOVE(P)   Num7_0        VABNSP
032000030107     C                   MOVE(P)   Num7_0        VATNSP
032100010201     C                   ELSE
032200010201     C                   ADD       1             errore
032300010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032400030311     C                             + ' ' + 'VABNSP VATNSP'
032500010201     C                   ENDIF
032600060104     C* RMN
032700060104     C***                EVAL      PiStr=%trim(%subst(vindta:1:13))
032800060104     C                   EVAL      PiStr=%trim(%subst(vindta:14:8))
032900030311     C                   EXSR      CHKNUM
033000030311     C                   IF        PiInt=*on
033100030311     C                   Z-ADD     PiVal         VABRMN
033200030311     C                   ELSE
033300030311     C                   ADD       1             errore
033400030311     C                   EVAL      vinmsg = %trimr(vinmsg)
033500030311     C                             + ' ' + 'VABRMN'
033600030311     C                   ENDIF
033700030226     C* NCL
033800030415     C                   EVAL      VABNCL = VABNCL + 1
033900030226     C* PKB
034000030415     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
034100030107     C                   EXSR      CHKNUM
034200030107     C                   IF        PiNum=*on
034300030415     C                   ADD       PiVal         VABPKB
034400030107     C                   ELSE
034500030107     C                   ADD       1             errore
034600030107     C                   EVAL      vinmsg = %trimr(vinmsg)
034700030107     C                             + ' ' + 'VABPKB'
034800030107     C                   ENDIF
034900030415     C* CAS
035000030415     C                   IF        %subst(vindta:210:13) <> '0000000000,00'
035100030310     C                   MOVEL     '1'           FlgCAS
035200101117     C                   EVAL      VABTIC = 'BM'
035300101117     C***                EVAL      VABTIC = 'OM'
035400101117     C***                EVAL      VABRMO = 'YAMAHA MOTOR ITALIA SPA'
035500101117     C***                EVAL      VABCMO = '20050'
035600030415     C                   EVAL      PiStr=%trim(%subst(vindta:210:13))
035700030310     C                   EXSR      CHKNUM
035800030310     C                   IF        PiNum=*on
035900030714     C                   ADD       PiVal         VABCAS
036000030310     C                   ELSE
036100030310     C                   SETON                                        32
036200030310     C                   Z-ADD     *zeros        VABCAS
036300030310     C                   EVAL      vinmsg = %trimr(vinmsg)
036400030310     C                             + ' ' + 'VABCAS'
036500030310     C                   ENDIF
036600030310     C                   ENDIF
036700030310     C*
036800030310     C* Considerazioni finali su CBO/CAS
036900030310     C                   IF        FlgCAS = '1'
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
039400101105     C*
039500101105     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
039600101105     C                   EVAL      VABCMR = %subst(wNomeFile:6:5)
039700101105     C                   EVAL      VABDCM = DATCOR
039800101105     C                   EVAL      VABDTS = DATCOR
039900101105     C                   EVAL      VABHMS = ORACOR
040000101105     C                   EVAL      VABCNT = 1
040100030310     C*
040200101105     C                   write     EDIVAB00
040300030226     C*
040400030226     C                   ENDSR
040500030226     C*----------------------------------------------------*
040600030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
040700030226     C*----------------------------------------------------*
040800030226     C     IMPVAT        BEGSR
040900030226     C*
041000030226     C                   EXSR      INZVAR
041100030226     C                   EXSR      DEFCAM
041200030226     C*
041300030226     C                   MOVEL     datcor        VATAAS
041400030226     C*
041500030415     C* Valorizzo il barcode
041600030415     C                   EVAL      VATNOT=%trim(%subst(vindta:14:8))
041700030226     C*
041800030226     C                   ENDSR
041900010201     C*----------------------------------------------------*
042000021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042100010201     C*----------------------------------------------------*
042200020305     C     WRIVAT        BEGSR
042300101105     C*
042400101105     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
042500101105     C                   EVAL      VATCMR = %subst(wNomeFile:6:5)
042600101105     C                   EVAL      VATCNT = 1
042700010201     C*
042800101105     C                   write     EDIVAT00
042900010201     C*
043000010201     C                   ENDSR
043100010202     C*----------------------------------------------------*
043200101105     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
043300010202     C*----------------------------------------------------*
043400020305     C     PREVAT        BEGSR
043500010202     C*
043600101105     C* Compongo il nome del membro da dare al EDIVATWR
043700010202     C                   eval      parmbr = vlrhdl
043800010202     C                   movel     'M'           parmbr
043900051230     C                   eval      parccm = vlrksc
044000010202     C                   eval      paropz = '1'
044100010202     C* Effettuo la chiamata al CLLE preposto
044200101105     C                   call(e)   'TITVEVTC'
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
053600101105     C                   eval      cmzfld = 'EDIVATWR'
053700010201     C                   eval      cmzmbd = vlrhdl
053800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
053900021009     C***                if        prmfir = *blanks
054000101105     C                   eval      cmzfla = 'EDIVAT0F'
054100101105     C                   eval      cmzmba = 'EDIVAT0F'
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
056500101105     C                   eval      cmzfla = 'EDIVAB0F'
056600101105     C                   eval      cmzmba = 'EDIVAB0F'
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
059200101105     C                   if        %open(edivabwr)
059300101105     C                   close     edivabwr
059400070411     C                   endif
059500101105     C                   if        %open(edivatwr)
059600101105     C                   close     edivatwr
059700070411     C                   endif
059800070411     C*
059900070411     C* Effettuo la chiamata al CLLE preposto
060000101105     C                   call(e)   'TITVEVTC'
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
062400101105     C                   movel     wn14          oracor            6 0          *ORA
062500000830     C                   z-add     wn8           g08dat
062600000830     C                   z-add     *zeros        g08inv
062700000830     C                   movel     '0'           g08err
062800000830     C                   call      'XSRDA8'
062900000830     C                   parm                    wlbda8
063000000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063100101105     C*
063200101105     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
063300101105     C                   move(p)   vlrMSG        wNomeFile        30
063400000830     C*
063500000613     C                   ENDSR
063600000613     C***
063700990908
063800030415     Otitv50p   E            riepilogo         2
063900990915     O                                              'Upload via Internet'
064000990915     O                                           +1 'Traduzione TIVIN00R -'
064100101105     O                                           +1 'EDIVABWR/EDIVATWR'
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
066300030415     Otitv50ps  E            testdett          2
066400000613     O                                              'Upload via Internet'
066500000613     O                                           +1 'Traduzione TIVIN00R -'
066600101105     O                                           +1 'EDIVABWR/EDIVATWR'
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
069400030415OVRPRTF FILE(TITV50P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069500030415OVRPRTF FILE(TITV50PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
069600030415DLTOVR FILE(TITV50P TITV50PS) LVL(*)
069700000512
069800000512
