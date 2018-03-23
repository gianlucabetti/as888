000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
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
009900050128     C                   call(e)   'TITVVTC'
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
013800030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
013900910830     C*--------------------------------------------------------
014000030226     C     RWFILE        BEGSR
014100990910     C*
014200990914     C                   if        not %open(tivin00r)
014300990908     C                   open      tivin00r
014400990914     C                   endif
014500021017     C                   if        not %open(fivabwwr)
014600021017     C                   open      fivabwwr
014700990914     C                   endif
014800021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014900020305     C                   exsr      prevat
015000010201     C*
015100010202     C                   if        chkcall = '0'
015200010202     C*
015300021017     C                   if        not %open(fivatwwr)
015400021017     C                   open      fivatwwr
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
017700030411     C                   clear                   fivab000
017800051230     C                   clear                   fivat000
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
021000021017     C                   if        %open(fivabwwr)
021100021017     C                   close     fivabwwr
021200990914     C                   endif
021300021017     C                   if        %open(fivatwwr)
021400021017     C                   close     fivatwwr
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
025200051230     C                   Z-ADD     100           VABCTR
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
035200030804     C                   EVAL      VABTIC = 'OM'
035300040628     C                   EVAL      VABRMO = 'YAMAHA MOTOR ITALIA SPA'
035400030804     C                   EVAL      VABCMO = '20050'
035500030415     C                   EVAL      PiStr=%trim(%subst(vindta:210:13))
035600030310     C                   EXSR      CHKNUM
035700030310     C                   IF        PiNum=*on
035800030714     C                   ADD       PiVal         VABCAS
035900030310     C                   ELSE
036000030310     C                   SETON                                        32
036100030310     C                   Z-ADD     *zeros        VABCAS
036200030310     C                   EVAL      vinmsg = %trimr(vinmsg)
036300030310     C                             + ' ' + 'VABCAS'
036400030310     C                   ENDIF
036500030310     C                   ENDIF
036600030310     C*
036700030310     C* Considerazioni finali su CBO/CAS
036800030310     C                   IF        FlgCAS = '1'
036900010205     C                   EVAL      VABCBO = '4'
037000010205     C                   ELSE
037100010205     C                   EVAL      VABCBO = '1'
037200010205     C                   ENDIF
037300020305     C*
037400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
037500011113     C                   EXSR      CHKIMPDIV
037600010202     C*
037700000801     C* Ebbene...
037800000801     C                   ADD       1             §CTRMO
037900010201     C                   IF        errore <> *zeros
038000000801     C                   ADD       1             §CTRNO
038100000801     C                   EVAL      recko = vindta
038200000801     C                   EXCEPT    rigadett
038300000801     C                   EVAL      vinflg = '2'
038400000801     C                   ELSE
038500010201     C                   ADD       1             §CTROKVB
038600000801     C                   ENDIF
038700000801     C*
038800000801     C                   ENDSR
038900030226     C*----------------------------------------------------*
039000030226     C*  SCARICAMENTO BUFFER RECORDS VAB
039100030226     C*----------------------------------------------------*
039200030226     C     WRIVAB        BEGSR
039300030310     C*
039400030310     C                   write     FIVAB000
039500030226     C*
039600030226     C                   ENDSR
039700030226     C*----------------------------------------------------*
039800030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
039900030226     C*----------------------------------------------------*
040000030226     C     IMPVAT        BEGSR
040100030226     C*
040200030226     C                   EXSR      INZVAR
040300030226     C                   EXSR      DEFCAM
040400030226     C*
040500030226     C                   MOVEL     datcor        VATAAS
040600030226     C*
040700030415     C* Valorizzo il barcode
040800030415     C                   EVAL      VATNOT=%trim(%subst(vindta:14:8))
040900030226     C*
041000030226     C                   ENDSR
041100010201     C*----------------------------------------------------*
041200021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
041300010201     C*----------------------------------------------------*
041400020305     C     WRIVAT        BEGSR
041500010201     C*
041600021017     C                   write     FIVAT000
041700010201     C*
041800010201     C                   ENDSR
041900010202     C*----------------------------------------------------*
042000021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100010202     C*----------------------------------------------------*
042200020305     C     PREVAT        BEGSR
042300010202     C*
042400021017     C* Compongo il nome del membro da dare al FIVATWWR
042500010202     C                   eval      parmbr = vlrhdl
042600010202     C                   movel     'M'           parmbr
042700051230     C                   eval      parccm = vlrksc
042800010202     C                   eval      paropz = '1'
042900010202     C* Effettuo la chiamata al CLLE preposto
043000050128     C                   call(e)   'TITVVTC'
043100010202     C                   parm                    parccm
043200010202     C                   parm                    parmbr
043300010202     C                   parm                    paropz
043400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500010202     C                   if        %error
043600010202     C                   movel     '1'           chkcall
043700010202     C                   else
043800010202     C                   movel     '0'           chkcall
043900010202     C                   endif
044000010202     C*
044100010202     C                   ENDSR
044200000801     C*----------------------------------------------------*
044300000801     C*  CONTROLLO NUMERICITA' CAMPI
044400000801     C*----------------------------------------------------*
044500000801     C     CHKNUM        BEGSR
044600000801     C*
044700000801     C                   call(e)   'ISNUMERIC'
044800000801     C                   PARM                    PiStr            30
044900030107     C                   PARM      ','           PiDecChr          1
045000000801     C                   PARM      *ZEROS        PiVal            30 9
045100000801     C                   PARM      '0'           PiInt             1
045200000801     C                   PARM      '0'           PiNum             1
045300000801     C                   IF        %error
045400000801     C                   EVAL      PiInt=*off
045500030107     C                   EVAL      PiNum=*off
045600000801     C                   ENDIF
045700000801     C*
045800000801     C                   ENDSR
045900000801     C***
046000000801
046100011113
046200011113     C*----------------------------------------------------*
046300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046400011113     C*----------------------------------------------------*
046500011113     C     CHKIMPDIV     BEGSR
046600011113     C*
046700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046800011113     C                   Z-ADD     *zeros        wrkDec            9 9
046900011113     C*
047000011113     C* Come prima cosa effettuo considerazioni sulla divisa
047100011113     C                   IF        vabIAS > *zeros
047200011113     C                   IF        vabVAS <> 'EUR'
047300011113     C                   EVAL      vabVAS =  'ITL'
047400011113     C                   ENDIF
047500011113     C                   ENDIF
047600011113     C*
047700011113     C                   IF        vabCAS > *zeros
047800011113     C                   IF        vabVCA <> 'EUR'
047900011113     C                   EVAL      vabVCA =  'ITL'
048000011113     C                   ENDIF
048100011113     C                   ENDIF
048200011113     C*
048300011113     C                   IF        vabVMD > *zeros
048400020305     C                   IF        vabVAD <> 'EUR'
048500011113     C                   EVAL      vabVAD =  'ITL'
048600011113     C                   ENDIF
048700011113     C                   ENDIF
048800011113     C*
048900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049000011113     C                   Z-ADD     vabIAS        wrkDec
049100011113     C                   IF        wrkDec > *zeros
049200011113     C                   IF        vabVAS = 'ITL'
049300011113     C                   EVAL      vabIAS = *zeros
049400011113     C                   ENDIF
049500011113     C                   ENDIF
049600011113     C*
049700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049800011113     C                   Z-ADD     vabCAS        wrkDec
049900011113     C                   IF        wrkDec > *zeros
050000011113     C                   IF        vabVCA = 'ITL'
050100011113     C                   EVAL      vabCAS = *zeros
050200011113     C                   ENDIF
050300011113     C                   ENDIF
050400011113     C*
050500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050600011113     C                   Z-ADD     vabVMD        wrkDec
050700011113     C                   IF        wrkDec > *zeros
050800011113     C                   IF        vabVAD = 'ITL'
050900011113     C                   EVAL      vabVMD = *zeros
051000011113     C                   ENDIF
051100011113     C                   ENDIF
051200011113     C*
051300011113     C                   ENDSR
051400011113     C***
051500021017
051600000801
051700000801
051800990920      /TITLE Invio dei dati al punto operativo.
051900010202     C     invio         BEGSR
052000990920     C*
052100021017     C* 1° invio FIVAT
052200010201     C                   reset                   dscmz
052300021017     C                   move      vlrpoi        cmzdst
052400021017     C                   eval      cmzfld = 'FIVATWWR'
052500010201     C                   eval      cmzmbd = vlrhdl
052600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052700021009     C***                if        prmfir = *blanks
052800021017     C                   eval      cmzfla = 'FIVAT00F'
052900021017     C                   eval      cmzmba = 'FIVAT00F'
053000021009     C***                else
053100021009     C***                eval      cmzfla = prmfir
053200021009     C***                eval      cmzmba = prmfir
053300021009     C***                endif
053400010201     C                   eval      cmznrr = *zeros
053500020305     C                   move      §ctrokvt      cmznrr
053600021018     C                   eval      cmzlba = vlrfl1
053700010201     C                   call(e)   'TIS711C'
053800010201     C                   parm                    dscmz
053900010201     C                   parm      *blanks       esito
054000010205     C                   if        %error
054100010205     C                             or cmzerr = '1'
054200010205     C                             or esito  = '1'
054300010205     C                   eval      wrkesito = '3'
054400010205     C                   else
054500010201     C*
054600021017     C* 2° invio FIVAB
054700010201     C                   reset                   dscmz
054800021017     C                   move      vlrpoi        cmzdst
054900010201     C                   eval      cmzfld = vlrfou
055000010201     C                   eval      cmzmbd = vlrhdl
055100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055200021009     C***                if        prmfir = *blanks
055300021017     C                   eval      cmzfla = 'FIVAB00F'
055400021017     C                   eval      cmzmba = 'FIVAB00F'
055500021009     C***                else
055600021009     C***                eval      cmzfla = prmfir
055700021009     C***                eval      cmzmba = prmfir
055800021009     C***                endif
055900010201     C                   eval      cmznrr = *zeros
056000010201     C                   move      §ctrokvb      cmznrr
056100021018     C                   eval      cmzlba = vlrfl1
056200010201     C                   call(e)   'TIS711C'
056300010201     C                   parm                    dscmz
056400010201     C                   parm      *blanks       esito
056500010201     C                   if        %error
056600010201     C                             or cmzerr = '1'
056700010201     C                             or esito  = '1'
056800010201     C                   eval      wrkesito = '3'
056900010201     C                   endif
057000010205     C                   endif
057100990920     C*
057200000613     C                   ENDSR
057300000613     C***
057400070411
057500070411     C     *pssr         BEGSR
057600070411     C*
057700070411     C                   if        %open(tivin00r)
057800070411     C                   close     tivin00r
057900070411     C                   endif
058000070411     C                   if        %open(fivabwwr)
058100070411     C                   close     fivabwwr
058200070411     C                   endif
058300070411     C                   if        %open(fivatwwr)
058400070411     C                   close     fivatwwr
058500070411     C                   endif
058600070411     C*
058700070411     C* Effettuo la chiamata al CLLE preposto
058800070411     C                   call(e)   'TITVVTC'
058900070411     C                   parm                    parccm
059000070411     C                   parm                    parmbr
059100070411     C                   parm      '2'           paropz
059200070411     C*
059300070411     C                   eval      wrkesito = '2'
059400070411     C*
059500070411     C                   seton                                        LR
059600070411     C*
059700070411     C                   ENDSR     '*CANCL'
059800070411     C***
059900070411
060000990910
060100000613     C     *inzsr        BEGSR
060200990910     C*
060300990910     C     *entry        plist
060400990920     C                   parm                    tivlrds
060500990921     C                   parm      wrkesito      esito
060600000724     C                   parm                    prmlit
060700000710     C                   parm                    prmfir
060800000613     C*
060900000830     C* CALCOLA LA DATA CORRENTE
061000000830     C                   time                    wn14             14 0
061100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061200000830     C                   z-add     wn8           g08dat
061300000830     C                   z-add     *zeros        g08inv
061400000830     C                   movel     '0'           g08err
061500000830     C                   call      'XSRDA8'
061600000830     C                   parm                    wlbda8
061700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061800000830     C*
061900000613     C                   ENDSR
062000000613     C***
062100990908
062200030415     Otitv50p   E            riepilogo         2
062300990915     O                                              'Upload via Internet'
062400990915     O                                           +1 'Traduzione TIVIN00R -'
062500021017     O                                           +1 'FIVABWWR/FIVATWWR'
062600010201     O                                           +1 'Report testate bolle'
062700990915     O          E            riepilogo   2
062800990915     O                       wrkdata
062900990915     O                       wrkora              +1
063000990915     O                       procname            +1
063100990915     O          E            riepilogo   2
063200990915     O                                              'Cliente..................:'
063300990915     O                       VABCCM        z     +1
063400990915     O          E            riepilogo   2
063500990920     O                                              'Riferimento Strategi.....:'
063600990920     O                       vlrhdl              +1
063700990915     O          E            riepilogo   2
063800990915     O                                              'Giusti...................:'
063900010201     O                       §CTROKVB      2   +  1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Sbagliati e corretti.....:'
064200971022     O                       §CTRMO        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e scartati.....:'
064500971022     O                       §CTRNO        2   +  1
064600000613
064700030415     Otitv50ps  E            testdett          2
064800000613     O                                              'Upload via Internet'
064900000613     O                                           +1 'Traduzione TIVIN00R -'
065000021017     O                                           +1 'FIVABWWR/FIVATWWR'
065100010201     O                                           +1 'Report testate bolle'
065200000616     O          E            testdett    3
065300000613     O                                           +2 'N° rec'
065400000613     O                                           +3 'Anteprima contenuto'
065500000616     O          E            rigadett    2
065600000613     O                       rrnum               +2
065700000621     O                       recko               +3
065800000616     O          E            findett     2
065900000613     O                       wrkdata
066000000613     O                       wrkora              +1
066100000613     O                       procname            +1
066200000616     O          E            findett     2
066300000613     O                                              'Cliente..................:'
066400000613     O                       VABCCM        z     +1
066500000616     O          E            findett     2
066600000613     O                                              'Riferimento Strategi.....:'
066700000613     O                       vlrhdl              +1
066800000616     O          E            findett     2
066900000613     O                                              'Giusti...................:'
067000010201     O                       §CTROKVB      2   +  1
067100000616     O          E            findett     2
067200000613     O                                              'Sbagliati e corretti.....:'
067300000613     O                       §CTRMO        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e scartati.....:'
067600000613     O                       §CTRNO        2   +  1
067700000512** CMD - COMANDI CL
067800030415OVRPRTF FILE(TITV50P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
067900030415OVRPRTF FILE(TITV50PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068000030415DLTOVR FILE(TITV50P TITV50PS) LVL(*)
068100000512
068200000512
